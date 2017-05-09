(load-file "prolog.clj")

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

  (<- (male george))
  
  (<- (parent b c))
  (<- (parent a b))

  (<- (mother cecilia a))
  (<- (father george a))

  (<- (gp A C) (parent A B) (parent B C))
  (<- (gp s f))

  (<- (ami a b))
  (<- (ami b c))
  (<- (ami c d))
  (<- (ami d e))
  (<- (ami e f))
  (<- (ami A C) (ami A B) (ami B C))

  (<- (c a b))
  (<- (b A B) (c B A))
  (<- (a A B) (b B A))

  (?- (parent X c))

  (?- (gp X c))

  (?- (gp X Y))

  (?- (a X Y))

  (?- (ami a f))

  (?- (mother cecilia C) (father george C))

  (?- (father F alexandra) (mother M alexandra))


  nil)

(load-db)
