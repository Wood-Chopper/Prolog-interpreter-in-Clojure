(ns tests
  (:use prolog))

(defn load-db []

  (<- (father george maria))
  (<- (father george howard))
  (<- (father george roger))
  (<- (father george laura))
  (<- (father albert tamara))
  (<- (father albert alexandra))
  (<- (father albert jessica))
  (<- (father roger brandon))
  (<- (father roger nadia))
  (<- (father bob frank))
  (<- (father bob anthony))

  (<- (mother cecilia howard))
  (<- (mother cecilia roger))
  (<- (mother cecilia laura))
  (<- (mother maria tamara))
  (<- (mother maria alexandra))
  (<- (mother maria jessica))
  (<- (mother sarah brandon))
  (<- (mother sarah nadia))
  (<- (mother laura frank))
  (<- (mother laura anthony))

  (<- (male george))
  (<- (male albert))
  (<- (male roger))
  (<- (male howard))
  (<- (male bob))
  (<- (male brandon))
  (<- (male frank))
  (<- (male anthony))

  (<- (female cecilia))
  (<- (female maria))
  (<- (female sarah))
  (<- (female laura))
  (<- (female tamara))
  (<- (female alexandra))
  (<- (female jessica))
  (<- (female nadia))

  (<- (parent Parent Child) (father Parent Child))
  (<- (parent Parent Child) (mother Parent Child))

  (<- (ancestor X Z) (parent X Z))
  (<- (ancestor X Z) (parent X Y) (ancestor Y Z))

  (<- (append (list) T T))
  (<- (append (list H T) L2 (list H TR))
      (append T L2 TR))

  ; e.g. (?- (append (list 1 (list 2 (list))) (list 3 (list 4 (list))) R))

  nil)

(load-db)
