(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)


(** An scoped dict is a hierarchy of scopes, where each scope is a mapping
  * from keys to values. Lookup prefers innermost scopes.
  *)
functor ScopedDict (Key: KEY) :>
sig
  type 'a t

  exception TopLevel
  val popScope: 'a t -> 'a t
  val newScope: 'a t -> 'a t
  val numScopes: 'a t -> int

  val emptyTopLevel: 'a t
  val topLevelFromList: (Key.t * 'a) list -> 'a t
  val insert: 'a t -> (Key.t * 'a) -> 'a t
  val remove: 'a t -> Key.t -> 'a t

  val contains: 'a t -> Key.t -> bool

  exception NotFound
  val lookup: 'a t -> Key.t -> 'a

end =
struct


  (** ======================================================================
    * A scope is just a dictionary. For now, I'm just using lists. Can make
    * this more efficient later if needed.
    *)
  structure Scope =
  struct
    type 'a t = (Key.t * 'a) list

    fun lookup scope key =
      case scope of
        [] => NONE
      | (key', value) :: scope' =>
          if Key.equal (key, key') then
            SOME value
          else
            lookup scope' key

    fun remove scope key =
      case scope of
        [] => []
      | (key', value) :: scope' =>
          if Key.equal (key, key') then
            remove scope' key
          else
            (key', value) :: remove scope' key

    fun insert scope (key, value) = (key, value) :: scope
  end
  (** ====================================================================== *)


  type 'a t = {curr: 'a Scope.t, prev: 'a Scope.t list}

  exception NotFound
  exception TopLevel

  val emptyTopLevel = {curr = [], prev = []}

  fun numScopes ({curr, prev}: 'a t) = 1 + List.length prev

  fun newScope ({curr, prev}: 'a t) =
    { curr = []
    , prev = curr :: prev
    }

  fun popScope ({curr, prev}: 'a t) =
    case prev of
      [] => raise TopLevel
    | curr' :: prev' =>
        { curr = curr'
        , prev = prev'
        }

  fun insert ({curr, prev}: 'a t) (key, value) =
    { curr = Scope.insert curr (key, value)
    , prev = prev
    }

  fun remove ({curr, prev}: 'a t) key =
    { curr = Scope.remove curr key
    , prev = prev
    }

  fun topLevelFromList elems =
    List.foldr (fn (elem, dict) => insert dict elem) emptyTopLevel elems

  fun contains (dict: 'a t) key =
    case Scope.lookup (#curr dict) key of
      SOME _ => true
    | NONE =>
        numScopes dict > 1
        andalso contains (popScope dict) key

  fun lookup (dict: 'a t) key =
    case Scope.lookup (#curr dict) key of
      SOME value => value
    | NONE =>
        if numScopes dict = 1 then
          raise NotFound
        else
          lookup (popScope dict) key

end
