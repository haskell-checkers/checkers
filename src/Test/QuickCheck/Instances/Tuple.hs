module Test.QuickCheck.Instances.Tuple where

import Test.QuickCheck
import Control.Monad

{- | Generates a 2-tuple using its arguments to generate the parts.
-}
(>*<) :: Gen a -> Gen b -> Gen (a,b)
x >*< y = liftM2 (,) x y

{- | Generates a 3-tuple using its arguments to generate the parts.
-}
(>**<) :: Gen a -> Gen b -> Gen c -> Gen (a,b,c)
(>**<) x y z = liftM3 (,,) x y z

{- | Generates a 4-tuple using its arguments to generate the parts.
-}
(>***<) :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a,b,c,d)
(>***<) x y z a = liftM4 (,,,) x y z a

{- | Generates a 5-tuple using its arguments to generate the parts.
-}
(>****<) :: Gen a -> Gen b -> Gen c -> Gen d -> Gen e -> Gen (a,b,c,d,e)
(>****<) x y z a b= liftM5 (,,,,) x y z a b

