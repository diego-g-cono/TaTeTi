import System.IO
import Data.List (intersperse, transpose)
import Text.Read (readMaybe)

tateti = do putStrLn "Introduce las dimensiones del tablero n x n (n>=3):" 
            n <- pedirNumero --captura el numero de fila y col
            if (n >= 3) then --verifica tener un tablero mayor o igual a 3
                do let tableroVacio = replicate n (replicate n ' ') --crea el tablero y lo guarda
                   jugar n tableroVacio 'X' --comienza el jugador X
            else do putStrLn "ERROR! El tamanio debe ser mayor o igual a 3"
                    tateti

jugar n tablero jugador = do imprimirTablero tablero 
                             putStrLn ("Turno del jugador " ++ [jugador] ++ ". Introduce las coordenadas (fila + ENTER y columna + ENTER):") 
                             fila <- pedirNumero -- captura la fila
                             col <- pedirNumero -- captura la columna
                             if movValido n tablero (fila-1) (col-1) then 
                                do let nuevoTablero = hacerJugada tablero (fila-1) (col-1) jugador
                                   if esGanador nuevoTablero jugador n then 
                                    do imprimirTablero nuevoTablero 
                                       putStrLn ("El ganador es: " ++ [jugador]) 
                                       jugarNuevamente 
                                   else if esEmpate nuevoTablero then 
                                    do imprimirTablero nuevoTablero 
                                       putStrLn "Empate!" 
                                       jugarNuevamente 
                                   else jugar n nuevoTablero (sigJugador jugador) 
                             else do putStrLn "Movimiento inválido, intenta nuevamente." 
                                     jugar n tablero jugador

pedirNumero = do num <- getLine --se verifica que la entrada sea un numero entero
                 case readMaybe num of
                    Just n | n > 0 -> return n --si es un Just int con la guarda preguntamos si es mayor a 0
                    _-> do putStrLn "ERROR! Debe ingresar un número entero mayor a 0."
                           pedirNumero
                                
imprimirTablero tablero = do let n = length tablero
                             putStrLn ("   " ++ concatMap (\i -> show i ++ "|") [1..n]) -- imprime el encabezado
                             imprimirFilas (zip [1..] tablero) (replicate ((n * 2) + 3) '-')
                              where formatoFila (i, fila) = show i ++ ": " ++ intersperse '|' fila -- formatea la fila para imprimir
                                    imprimirFilas [] _ = return ()
                                    imprimirFilas [fila] _ = putStrLn (formatoFila fila) -- ultima fila sin separador
                                    imprimirFilas (fila:filas) separador = do putStrLn (formatoFila fila)
                                                                              putStrLn separador
                                                                              imprimirFilas filas separador

hacerJugada tablero fila col jugador = take fila tablero ++ [take col (tablero !! fila) ++ [jugador] ++ drop (col + 1) (tablero !! fila)] ++ drop (fila + 1) tablero --reemplaza con X o O en el tablero

movValido n tablero fila col = fila >= 0 && fila < n && col >= 0 && col < n && (tablero !! fila !! col) == ' ' --verifica que este en rango y que este disponible

sigJugador 'X' = 'O' --cambia de turno
sigJugador 'O' = 'X' --cambia de turno

esGanador tablero jugador n = any (all (== jugador)) (filas ++ cols ++ diags) --any devuelve true si algun resultado del all da true
                                where filas = tablero 
                                      cols = transpose tablero --devuelve las traspuesta del tablero para tener en las listas las columnas
                                      diags = [diag tablero, diag (map reverse tablero)] --devuelve las dos diagonales del tablero
                                      diag b = [b !! i !! i | i <- [0..n-1]] --devuelve una diagonal

esEmpate tablero = all (all (/= ' ')) tablero --la funcion all se aplica sobre todos los elementos y verifica que este completo el tablero

jugarNuevamente = do putStrLn "¿Quieres jugar nuevamente? (s/n)"
                     respuesta <- getLine
                     if respuesta == "s"
                        then main
                        else return ()

main = do putStrLn "Bienvenido a Tateti!" 
          tateti