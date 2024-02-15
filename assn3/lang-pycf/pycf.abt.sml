structure PyCF :> PYCF =
struct

  structure Int =
  struct type t = int val equal: t * t -> bool = op= val toString = Int.toString end

  structure Bool =
  struct
    type t = bool
    val equal: t * t -> bool = op=
    val toString = Bool.toString
  end

  structure ClassOps =
  struct
    datatype class = Class'Bool | Class'Int | Class'List | Class'Fun

    fun class_bind x i class =
      (case class of
         Class'Bool => Class'Bool
       | Class'Int => Class'Int
       | Class'List => Class'List
       | Class'Fun => Class'Fun)

    fun class_unbind x i class =
      (case class of
         Class'Bool => Class'Bool
       | Class'Int => Class'Int
       | Class'List => Class'List
       | Class'Fun => Class'Fun)

    fun class_internal_aequiv (class1, class2) =
      (case (class1, class2) of
         (Class'Bool, Class'Bool) => true
       | (Class'Int, Class'Int) => true
       | (Class'List, Class'List) => true
       | (Class'Fun, Class'Fun) => true
       | _ => false)
  end

  datatype class = Bool | Int | List | Fun

  fun class_view_in vars class =
    (case class of
       Bool => (ClassOps.Class'Bool, [])
     | Int => (ClassOps.Class'Int, [])
     | List => (ClassOps.Class'List, [])
     | Fun => (ClassOps.Class'Fun, []))

  fun class_view_out vars class =
    (case class of
       ClassOps.Class'Bool => (Bool, [])
     | ClassOps.Class'Int => (Int, [])
     | ClassOps.Class'List => (List, [])
     | ClassOps.Class'Fun => (Fun, []))

  structure Class = struct datatype class = datatype class type t = class end

  fun class_aequiv (class1, class2) =
    (case (class1, class2) of
       (Bool, Bool) => true
     | (Int, Int) => true
     | (List, List) => true
     | (Fun, Fun) => true
     | _ => false)

  fun class_toString class =
    (case class of
       Class.Bool => "Bool"
     | Class.Int => "Int"
     | Class.List => "List"
     | Class.Fun => "Fun")

  structure Class =
  struct
    open Class
    val toString = class_toString
    val internal_aequiv = ClassOps.class_internal_aequiv
    val aequiv = class_aequiv
  end

  structure ObjectOps =
  struct
    datatype object_oper =
      Object'Bool of Bool.t
    | Object'If of object * object * object
    | Object'Int of Int.t
    | Object'Plus of object * object
    | Object'LEq of object * object
    | Object'List of object list
    | Object'Index of object * object
    | Object'Len of object
    | Object'Fun of string * (string * object)
    | Object'Ap of object * object
    | Object'Let of object * (string * object)
    | Object'IsInstance of object * ClassOps.class
    withtype object = object_oper Abt.t

    fun object_oper_bind x i object =
      (case object of
         (Object'Bool bool'1) => (Object'Bool bool'1)
       | (Object'If (object'2, object'3, object'4)) =>
           (Object'If
              ( (Abt.bind object_oper_bind x i object'2)
              , (Abt.bind object_oper_bind x i object'3)
              , (Abt.bind object_oper_bind x i object'4)
              ))
       | (Object'Int int'5) => (Object'Int int'5)
       | (Object'Plus (object'6, object'7)) =>
           (Object'Plus
              ( (Abt.bind object_oper_bind x i object'6)
              , (Abt.bind object_oper_bind x i object'7)
              ))
       | (Object'LEq (object'8, object'9)) =>
           (Object'LEq
              ( (Abt.bind object_oper_bind x i object'8)
              , (Abt.bind object_oper_bind x i object'9)
              ))
       | (Object'List list'11) =>
           (Object'List
              (List.map
                 (fn object'10 => (Abt.bind object_oper_bind x i object'10))
                 list'11))
       | (Object'Index (object'12, object'13)) =>
           (Object'Index
              ( (Abt.bind object_oper_bind x i object'12)
              , (Abt.bind object_oper_bind x i object'13)
              ))
       | (Object'Len object'14) =>
           (Object'Len (Abt.bind object_oper_bind x i object'14))
       | (Object'Fun (object'15, (object'16, object'17))) =>
           (Object'Fun
              ( object'15
              , (object'16, (Abt.bind object_oper_bind x i object'17))
              ))
       | (Object'Ap (object'18, object'19)) =>
           (Object'Ap
              ( (Abt.bind object_oper_bind x i object'18)
              , (Abt.bind object_oper_bind x i object'19)
              ))
       | (Object'Let (object'20, (object'21, object'22))) =>
           (Object'Let
              ( (Abt.bind object_oper_bind x i object'20)
              , (object'21, (Abt.bind object_oper_bind x i object'22))
              ))
       | (Object'IsInstance (object'23, class'24)) =>
           (Object'IsInstance
              ( (Abt.bind object_oper_bind x i object'23)
              , (ClassOps.class_bind x i class'24)
              )))

    fun object_oper_unbind x i object =
      (case object of
         (Object'Bool bool'1) => (Object'Bool bool'1)
       | (Object'If (object'2, object'3, object'4)) =>
           (Object'If
              ( (Abt.unbind object_oper_unbind x i object'2)
              , (Abt.unbind object_oper_unbind x i object'3)
              , (Abt.unbind object_oper_unbind x i object'4)
              ))
       | (Object'Int int'5) => (Object'Int int'5)
       | (Object'Plus (object'6, object'7)) =>
           (Object'Plus
              ( (Abt.unbind object_oper_unbind x i object'6)
              , (Abt.unbind object_oper_unbind x i object'7)
              ))
       | (Object'LEq (object'8, object'9)) =>
           (Object'LEq
              ( (Abt.unbind object_oper_unbind x i object'8)
              , (Abt.unbind object_oper_unbind x i object'9)
              ))
       | (Object'List list'11) =>
           (Object'List
              (List.map
                 (fn object'10 => (Abt.unbind object_oper_unbind x i object'10))
                 list'11))
       | (Object'Index (object'12, object'13)) =>
           (Object'Index
              ( (Abt.unbind object_oper_unbind x i object'12)
              , (Abt.unbind object_oper_unbind x i object'13)
              ))
       | (Object'Len object'14) =>
           (Object'Len (Abt.unbind object_oper_unbind x i object'14))
       | (Object'Fun (object'15, (object'16, object'17))) =>
           (Object'Fun
              ( object'15
              , (object'16, (Abt.unbind object_oper_unbind x i object'17))
              ))
       | (Object'Ap (object'18, object'19)) =>
           (Object'Ap
              ( (Abt.unbind object_oper_unbind x i object'18)
              , (Abt.unbind object_oper_unbind x i object'19)
              ))
       | (Object'Let (object'20, (object'21, object'22))) =>
           (Object'Let
              ( (Abt.unbind object_oper_unbind x i object'20)
              , (object'21, (Abt.unbind object_oper_unbind x i object'22))
              ))
       | (Object'IsInstance (object'23, class'24)) =>
           (Object'IsInstance
              ( (Abt.unbind object_oper_unbind x i object'23)
              , (ClassOps.class_unbind x i class'24)
              )))

    fun object_oper_aequiv (object1, object2) =
      (case (object1, object2) of
         ((Object'Bool bool'1), (Object'Bool bool'2)) =>
           (Bool.equal (bool'1, bool'2))
       | ( (Object'If (object'1, object'3, object'5))
         , (Object'If (object'2, object'4, object'6))
         ) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso ((Abt.aequiv object_oper_aequiv) (object'3, object'4))
            andalso ((Abt.aequiv object_oper_aequiv) (object'5, object'6)))
       | ((Object'Int int'1), (Object'Int int'2)) => (Int.equal (int'1, int'2))
       | ( (Object'Plus (object'1, object'3))
         , (Object'Plus (object'2, object'4))
         ) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso ((Abt.aequiv object_oper_aequiv) (object'3, object'4)))
       | ((Object'LEq (object'1, object'3)), (Object'LEq (object'2, object'4))) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso ((Abt.aequiv object_oper_aequiv) (object'3, object'4)))
       | ((Object'List list'3), (Object'List list'4)) =>
           (ListPair.allEq
              (fn (object'1, object'2) =>
                 ((Abt.aequiv object_oper_aequiv) (object'1, object'2)))
              (list'3, list'4))
       | ( (Object'Index (object'1, object'3))
         , (Object'Index (object'2, object'4))
         ) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso ((Abt.aequiv object_oper_aequiv) (object'3, object'4)))
       | ((Object'Len object'1), (Object'Len object'2)) =>
           ((Abt.aequiv object_oper_aequiv) (object'1, object'2))
       | ((Object'Fun (_, (_, object'1))), (Object'Fun (_, (_, object'2)))) =>
           (true
            andalso
            (true andalso ((Abt.aequiv object_oper_aequiv) (object'1, object'2))))
       | ((Object'Ap (object'1, object'3)), (Object'Ap (object'2, object'4))) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso ((Abt.aequiv object_oper_aequiv) (object'3, object'4)))
       | ( (Object'Let (object'1, (_, object'3)))
         , (Object'Let (object'2, (_, object'4)))
         ) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso
            (true andalso ((Abt.aequiv object_oper_aequiv) (object'3, object'4))))
       | ( (Object'IsInstance (object'1, class'3))
         , (Object'IsInstance (object'2, class'4))
         ) =>
           (((Abt.aequiv object_oper_aequiv) (object'1, object'2))
            andalso (Class.internal_aequiv (class'3, class'4)))
       | _ => false)
  end

  structure Object =
  struct
    type objectVar = Temp.t
    type object = ObjectOps.object
    type t = object

    structure Var = Temp

    datatype view =
      Var of objectVar
    | Bool of Bool.t
    | If of object * object * object
    | Int of Int.t
    | Plus of object * object
    | LEq of object * object
    | List of object list
    | Index of object * object
    | Len of object
    | Fun of objectVar * (objectVar * object)
    | Ap of object * object
    | Let of object * (objectVar * object)
    | IsInstance of object * Class.t

    fun view_in vars object =
      (case object of
         (Var x) => (Abt.Var x)
       | (Bool bool'1) =>
           (Abt.Oper (ObjectOps.Object'Bool ((fn (x, _) => x) (bool'1, []))))
       | (If (object'2, object'3, object'4)) =>
           (Abt.Oper (ObjectOps.Object'If ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'3 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'4 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Int int'5) =>
           (Abt.Oper (ObjectOps.Object'Int ((fn (x, _) => x) (int'5, []))))
       | (Plus (object'6, object'7)) =>
           (Abt.Oper (ObjectOps.Object'Plus ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'6 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (LEq (object'8, object'9)) =>
           (Abt.Oper (ObjectOps.Object'LEq ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'8 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'9 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (List list'11) =>
           (Abt.Oper (ObjectOps.Object'List ((fn (x, _) => x)
              let
                val (ts, vars') = (ListPair.unzip
                  (List.map
                     (fn object'10 =>
                        ( (List.foldl
                             (fn (x, acc) =>
                                (Abt.into ObjectOps.object_oper_bind
                                   (Abt.Binding (x, acc)))) object'10 vars)
                        , []
                        )) list'11))
              in
                (ts, (List.concat vars'))
              end)))
       | (Index (object'12, object'13)) =>
           (Abt.Oper (ObjectOps.Object'Index ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'12 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'13 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Len object'14) =>
           (Abt.Oper (ObjectOps.Object'Len ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ObjectOps.object_oper_bind
                         (Abt.Binding (x, acc)))) object'14 vars)
              , []
              ))))
       | (Fun (object'15, (object'16, object'17))) =>
           (Abt.Oper (ObjectOps.Object'Fun ((fn (x, _) => x)
              let
                val (t, vars') = let val var = object'15
                                 in ((Temp.toUserString var), [var])
                                 end
                val vars = (vars' @ vars)
                val (t', vars') =
                  let
                    val (t, vars') = let val var = object'16
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ObjectOps.object_oper_bind
                                 (Abt.Binding (x, acc)))) object'17 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t, t'), vars')
              end)))
       | (Ap (object'18, object'19)) =>
           (Abt.Oper (ObjectOps.Object'Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'18 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'19 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Let (object'20, (object'21, object'22))) =>
           (Abt.Oper (ObjectOps.Object'Let ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'20 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = object'21
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ObjectOps.object_oper_bind
                                 (Abt.Binding (x, acc)))) object'22 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (IsInstance (object'23, class'24)) =>
           (Abt.Oper (ObjectOps.Object'IsInstance ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ObjectOps.object_oper_bind
                             (Abt.Binding (x, acc)))) object'23 vars)
                  , []
                  )
                val (t1, vars'1) = (class_view_in vars class'24)
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))))

    fun oper_view_out vars object =
      (case object of
         (ObjectOps.Object'Bool bool'1) =>
           (Bool ((fn (x, _) => x) (bool'1, [])))
       | (ObjectOps.Object'If (object'2, object'3, object'4)) =>
           (If ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'3 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'4 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ObjectOps.Object'Int int'5) => (Int ((fn (x, _) => x) (int'5, [])))
       | (ObjectOps.Object'Plus (object'6, object'7)) =>
           (Plus ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'6 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ObjectOps.Object'LEq (object'8, object'9)) =>
           (LEq ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'8 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'9 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ObjectOps.Object'List list'11) =>
           (List ((fn (x, _) => x)
              let
                val (ts, vars') = (ListPair.unzip
                  (List.map
                     (fn object'10 =>
                        ( (List.foldr
                             (fn (x, acc) =>
                                let
                                  val (Abt.Binding (_, acc')) =
                                    (Abt.out ObjectOps.object_oper_unbind
                                       (Abt.unbind ObjectOps.object_oper_unbind
                                          x ~1 acc))
                                in
                                  acc'
                                end) object'10 vars)
                        , []
                        )) list'11))
              in
                (ts, (List.concat vars'))
              end))
       | (ObjectOps.Object'Index (object'12, object'13)) =>
           (Index ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'12 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'13 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ObjectOps.Object'Len object'14) =>
           (Len ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ObjectOps.object_oper_unbind
                             (Abt.unbind ObjectOps.object_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) object'14 vars)
              , []
              )))
       | (ObjectOps.Object'Fun (object'15, (object'16, object'17))) =>
           (Fun ((fn (x, _) => x)
              let
                val (t, vars') = let val x = (Temp.new object'15)
                                 in (x, [x])
                                 end
                val vars = (vars' @ vars)
                val (t', vars') =
                  let
                    val (t, vars') = let val x = (Temp.new object'16)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ObjectOps.object_oper_unbind
                                     (Abt.unbind ObjectOps.object_oper_unbind x
                                        ~1 acc))
                              in
                                acc'
                              end) object'17 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t, t'), vars')
              end))
       | (ObjectOps.Object'Ap (object'18, object'19)) =>
           (Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'18 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'19 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ObjectOps.Object'Let (object'20, (object'21, object'22))) =>
           (Let ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'20 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new object'21)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ObjectOps.object_oper_unbind
                                     (Abt.unbind ObjectOps.object_oper_unbind x
                                        ~1 acc))
                              in
                                acc'
                              end) object'22 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ObjectOps.Object'IsInstance (object'23, class'24)) =>
           (IsInstance ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ObjectOps.object_oper_unbind
                                 (Abt.unbind ObjectOps.object_oper_unbind x ~1
                                    acc))
                          in
                            acc'
                          end) object'23 vars)
                  , []
                  )
                val (t1, vars'1) = (class_view_out vars class'24)
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))

    fun view_out t =
      (case t of
         (Abt.Var x) => (Var x)
       | (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ObjectOps.object_oper_bind (view_in [] v))
    fun out object =
      (view_out (Abt.out ObjectOps.object_oper_unbind object))

    val Var' = (into o Var)
    val Bool' = (into o Bool)
    val If' = (into o If)
    val Int' = (into o Int)
    val Plus' = (into o Plus)
    val LEq' = (into o LEq)
    val List' = (into o List)
    val Index' = (into o Index)
    val Len' = (into o Len)
    val Fun' = (into o Fun)
    val Ap' = (into o Ap)
    val Let' = (into o Let)
    val IsInstance' = (into o IsInstance)
  end

  fun object_toString object =
    (case (Object.out object) of
       (Object.Var x) => (Object.Var.toString x)
     | (Object.Bool bool'1) => ("(Bool " ^ (Bool.toString bool'1) ^ ")")
     | (Object.If (object'2, object'3, object'4)) =>
         ("(If "
          ^
          ("(" ^ (object_toString object'2) ^ ", " ^ (object_toString object'3)
           ^ ", " ^ (object_toString object'4) ^ ")") ^ ")")
     | (Object.Int int'5) => ("(Int " ^ (Int.toString int'5) ^ ")")
     | (Object.Plus (object'6, object'7)) =>
         ("(Plus "
          ^
          ("(" ^ (object_toString object'6) ^ ", " ^ (object_toString object'7)
           ^ ")") ^ ")")
     | (Object.LEq (object'8, object'9)) =>
         ("(LEq "
          ^
          ("(" ^ (object_toString object'8) ^ ", " ^ (object_toString object'9)
           ^ ")") ^ ")")
     | (Object.List list'11) =>
         ("(List "
          ^
          ("["
           ^
           String.concatWith ", "
             (List.map (fn object'10 => (object_toString object'10)) list'11)
           ^ "]") ^ ")")
     | (Object.Index (object'12, object'13)) =>
         ("(Index "
          ^
          ("(" ^ (object_toString object'12) ^ ", "
           ^ (object_toString object'13) ^ ")") ^ ")")
     | (Object.Len object'14) => ("(Len " ^ (object_toString object'14) ^ ")")
     | (Object.Fun (object'15, (object'16, object'17))) =>
         ("(Fun "
          ^
          ("(" ^ (Object.Var.toString object'15) ^ " . "
           ^
           ("(" ^ (Object.Var.toString object'16) ^ " . "
            ^ (object_toString object'17) ^ ")") ^ ")") ^ ")")
     | (Object.Ap (object'18, object'19)) =>
         ("(Ap "
          ^
          ("(" ^ (object_toString object'18) ^ ", "
           ^ (object_toString object'19) ^ ")") ^ ")")
     | (Object.Let (object'20, (object'21, object'22))) =>
         ("(Let "
          ^
          ("(" ^ (object_toString object'20) ^ ", "
           ^
           ("(" ^ (Object.Var.toString object'21) ^ " . "
            ^ (object_toString object'22) ^ ")") ^ ")") ^ ")")
     | (Object.IsInstance (object'23, class'24)) =>
         ("(IsInstance "
          ^
          ("(" ^ (object_toString object'23) ^ ", " ^ (Class.toString class'24)
           ^ ")") ^ ")"))

  fun object_subst t x object =
    (case (Object.out object) of
       (Object.Var y) =>
         (if (Object.Var.equal (x, y)) then t else (Object.Var' y))
     | (Object.Bool bool'1) => (Object.Bool' bool'1)
     | (Object.If (object'2, object'3, object'4)) =>
         (Object.If'
            ( (object_subst t x object'2)
            , (object_subst t x object'3)
            , (object_subst t x object'4)
            ))
     | (Object.Int int'5) => (Object.Int' int'5)
     | (Object.Plus (object'6, object'7)) =>
         (Object.Plus'
            ((object_subst t x object'6), (object_subst t x object'7)))
     | (Object.LEq (object'8, object'9)) =>
         (Object.LEq' ((object_subst t x object'8), (object_subst t x object'9)))
     | (Object.List list'11) =>
         (Object.List'
            (List.map (fn object'10 => (object_subst t x object'10)) list'11))
     | (Object.Index (object'12, object'13)) =>
         (Object.Index'
            ((object_subst t x object'12), (object_subst t x object'13)))
     | (Object.Len object'14) => (Object.Len' (object_subst t x object'14))
     | (Object.Fun (object'15, (object'16, object'17))) =>
         (Object.Fun' (object'15, (object'16, (object_subst t x object'17))))
     | (Object.Ap (object'18, object'19)) =>
         (Object.Ap'
            ((object_subst t x object'18), (object_subst t x object'19)))
     | (Object.Let (object'20, (object'21, object'22))) =>
         (Object.Let'
            ( (object_subst t x object'20)
            , (object'21, (object_subst t x object'22))
            ))
     | (Object.IsInstance (object'23, class'24)) =>
         (Object.IsInstance'
            ( (object_subst t x object'23)
            , ((fn _ => (fn _ => (fn t => t))) t x class'24)
            )))

  structure Object =
  struct
    open Object
    val toString = object_toString
    val aequiv = (Abt.aequiv ObjectOps.object_oper_aequiv)
    val subst = object_subst
  end
end
