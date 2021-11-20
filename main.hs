import System.Environment
import System.Random (randomRIO)




calcular :: (Floating a,Ord a) => [(a,a)] -> a -> [[(a,a)]]

calcular [(x1,y1)] _ = []
calcular ((x1,y1):xs) r = (otra ([(x1,y1)]++xs) r ) ++ calcular xs r



otra :: (Floating a, Ord a) => [(a,a)] -> a ->[[(a,a)]]

otra [(x1,y1)] _ = []
otra  (x:y:xs) r =
--calcular la distancia entre las dos particulas, luego compara la distancia con la suma de los radios (los radios son iguales = r+r = 2r)
--si hay colision se guardan los puntos y se sigue buscando
	let 
		distancia = sqrt ((fst(y)-fst(x))^2+(snd(y)-snd(x))^2)
	in (if distancia < 2*r then ([[x]++[y]] ++ otra ([x] ++ xs) r) else otra ([x] ++ xs) r )
	

generarAleatorios :: Float -> Float -> IO([Float])
generarAleatorios 0 _= return []
generarAleatorios n l= do
	r  <- randomRIO (0,l)
	rs <- generarAleatorios (n-1) l
	return (r:rs) 

main:: IO()
main = do
	[arg1,arg2,arg3,arg4,arg5] <- getArgs
	let 
		m = read arg1 :: Float
		r = read arg2 :: Float
		l = read arg3 :: Float
		q = read arg4 :: Float
		n = read arg5 :: Float
		
	x <- generarAleatorios n l
	y <- generarAleatorios n l
	
	let puntosAleatorios = zip x y
	print puntosAleatorios
	let z= calcular puntosAleatorios r
	print "colisiones: "
	print z
