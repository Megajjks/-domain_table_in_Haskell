import Data.Char
import Text.Printf
import System.IO

-- importar ficher
main = do
    -- contents <- readFile "dominio.txt" :: IO Int
    -- putStr contents
    putStrLn "Tabla de dominio de F(x)=x^2-2?"
    putStrLn "Valor mininimo"
    minVal <- readLn :: IO Int
    putStrLn "Valor maximo"
    maxVal <- readLn :: IO Int
    tablaFun "imagen.txt" [minVal .. maxVal]
    putStrLn ("Puedes ver este resultado en el archivo imagen.txt")
    muestraContenidoFichero "imagen.txt" 



-- Declarar función
funcdominio::Int->Int
funcdominio (n) = (n*n) - 2
 

-- Generar un nuevo txt con los resultados
tablaFun :: FilePath -> [Int] -> IO ()
tablaFun f ns = do
  writeFile f (tablaFunAux ns)

tablaFunAux :: [Int] -> String
tablaFunAux ns =
     linea
  ++ cabecera
  ++ linea
  ++ concat [printf "| %2d | %10d  |\n" n x
            | n <- ns
            , let x = funcdominio(n) ::Int]
  ++ linea

linea, cabecera :: String
linea    = "+----+-------------+\n"
cabecera = "| n  | f(x)=x^2-2  |\n"

-- imprimir resultado del fichero
muestraContenidoFichero :: FilePath -> IO ()
muestraContenidoFichero f = do
  cs <- readFile f
  putStrLn cs