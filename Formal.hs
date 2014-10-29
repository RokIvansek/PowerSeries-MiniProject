module Training.Formal
	where

infixr 5 :+:
data Formal a = a :+: (Formal a)

-- | Funkcija head vrne prvi koeficient vrste
head :: Formal a -> a
head (x :+: xs) = x

-- | Tail vrne vse ostale, razen prvega koeficienta vrste
tail :: Formal a -> Formal a
tail (x :+: xs) = xs

-- | Pretvori seznam v Formal
fromList :: Num a => [a] -> Formal a
fromList x
	| null x 		= fromList $ repeat 0
	| otherwise 	= (Prelude.head x) :+: (fromList (Prelude.tail x))

-- | Pretvori Formal v seznam
toList :: Formal a -> [a]
toList (x :+: xs) = x : (toList xs)

-- | Formalne vrste so Functor
instance Functor Formal where
	fmap f (x :+: xs) = (f x) :+: (fmap f xs)
	
-- | Formalne vrste na zaslonu prikažemo kot sezname. Vzamemo prvih 20 koeficientov.
instance Show a => Show (Formal a) where
	show x = show $ take 20 $ toList x

-- | Da lahko definiramo množenje vrst, najprej definiramo množenje vrste s skalarjems
infixl 7 .* 									
(.*) :: Num a => a -> Formal a -> Formal a 		
c .* (f :+: fs) = c*f :+: c.*fs 	

-- | Formalne vrste lahko seštevamo, odštevamo in množimo.
-- | Za minimalno definicijo potrevujemo še absolutno vrednost, predznak ter kako iz tipa Integer dobimo vrsto.
instance Num a => Num (Formal a) where
	x + y 					= fromList $ zipWith (+) (toList x) (toList y)
	x - y 					= fromList $ zipWith (-) (toList x) (toList y)
	(f :+: fs) * (g :+: gs) = f*g :+: (f.*gs + fs*(g :+: gs))
	abs (x :+: xs)			= (abs x) :+: (abs xs)
	fromInteger x			= fromList([fromInteger x])
	signum (x :+: xs)		= (signum x) :+: (signum xs)
	
-- | Formalne vrste lahko med sabo delimo.
-- | Če je prvi člen delitelja 0 in prvi člen deljenca neničeln deljenje ne uspe.
instance (Eq a, Fractional a) => Fractional (Formal a) where
	fromRational x 	= fromList $ [fromRational x]
	(0 :+: fs) / (0 :+: gs) = fs/gs
	(f :+: fs) / (g :+: gs) = let q =f/g in
		q :+: (fs - q.*gs)/(g :+: gs)	
	
-- | Kompozicija dveh vrst kot desno asociativna infiksna funkcija.
infixr 9 .:.
(.:.) :: (Num a, Eq a) => Formal a -> Formal a -> Formal a
(f :+: fs) .:. (0 :+: gs) = f :+: (gs*(fs Training.Formal..:. (0 :+: gs)))

-- | compose naredi isto kot .:. le da jo v izraz postavimo na začetek.
compose :: (Num a, Eq a) => Formal a -> Formal a -> Formal a
compose (f :+: fs) (0 :+: gs) = f :+: gs*(compose fs (0 :+: gs))

-- | Vrne inverz vrste.
revert (0 :+: fs) = rs where
	rs = 0 :+: (1/(compose fs rs))

-- | Izračuna odvod.
deriv (f :+: fs) = (deriv1 fs 1) where
	deriv1 (g :+: gs) n = (n*g) :+: (deriv1 gs (n+1))
	
-- | Izračuna določeni integral od 0 do x.
integral fs = 0 :+: (int1 (fs) (1)) where
	int1 (g :+: gs) (n) = (g/n) :+: int1 (gs) (n+1)

-- | Eksponentna funkcija, sinus in cosinus.
expx, cosx, sinx:: Fractional a => Formal a
expx = 1 + (integral expx)
sinx = integral cosx
cosx = 1 - (integral sinx)










	
	