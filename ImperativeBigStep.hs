
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | ThreeTimes C   ---- Executa o comando C 3 vezes
    | DoWhile C B --- DoWhile C B: executa C enquanto B é verdadeiro
    | Loop C E      ---- Loop E C: executa E vezes o comando C 
    | Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
    | ExecWhile E E C -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C 
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep(Div e1 e2,s) = ebigStep (e1,s) `div` ebigStep (e2,s)

---------------------------------
---
--- COMEÇO DOS TESTES ARITIMÉTICOS
---
---------------------------------

exArit :: Memoria
exArit = [ ("x", 10), ("y",5), ("z",0)]

-- NUMEROS E VARIÁVEIS
testeA1a = ebigStep (Num 15, exArit)                         -- ESPERADO: 15
testeA1b = ebigStep (Var "x", exArit)                        -- ESPERADO: 10
testeA1c = ebigStep (Var "y", exArit)                        -- ESPERADO: 5

-- SOMAS
testeA2a = ebigStep (Soma (Num 5) (Num 3), exArit)           -- 5 + 3 = 8; ESPERADO: 8
testeA2b = ebigStep (Soma (Var "x") (Num 2), exArit)         -- 10 + 2 = 12; ESPERADO: 12
testeA2c = ebigStep (Soma (Var "x") (Var "y"), exArit)       -- 10 + 5 = 15; ESPERADO: 15

-- SUBTRAÇÕES
testeA3a = ebigStep (Sub (Num 10) (Num 4), exArit)           -- 10 - 4 = 6; ESPERADO: 6
testeA3b = ebigStep (Sub (Var "x") (Num 5), exArit)          -- 10 - 5 = 5; ESPERADO: 5
testeA3c = ebigStep (Sub (Var "y") (Var "x"), exArit)       -- 5 - 10 = -5; ESPERADO: -5

-- MULTIPLICAÇÕES
testeA4a = ebigStep (Mult (Num 3) (Num 6), exArit)           -- 3 * 6 = 18; ESPERADO: 18
testeA4b = ebigStep (Mult (Var "y") (Num 2), exArit)         -- 5 * 2 = 10; ESPERADO: 10
testeA4c = ebigStep (Mult (Var "x") (Var "y"), exArit)       -- 10 * 5 = 50; ESPERADO: 50

-- DIVISÕES
testeA5a = ebigStep (Div (Num 10) (Num 2), exArit)           -- 10 / 2 = 5; ESPERADO: 5
testeA5b = ebigStep (Div (Var "x") (Num 3), exArit)          -- 10 / 3 = 3; ESPERADO: 3 (Divisão sem decimal)
testeA5c = ebigStep (Div (Var "x") (Var "y"), exArit)       -- 10 / 5 = 2; ESPERADO: 2
testeA5d = ebigStep (Div (Num 10) (Num 0), exArit)        -- ESPERADO: "Exception: divide by zero" 

-- EXPRESSÕES COMPLEXAS (MULTIPLAS CONTAS)
testeA6a = ebigStep (Soma (Mult (Num 2) (Num 3)) (Sub (Num 10) (Num 5)), exArit)  -- (2 * 3) + (10 - 5) = 6 + 5 = 11; ESPERADO: 11
testeA6b = ebigStep (Div (Soma (Var "x") (Var "y")) (Num 3), exArit)                              -- (10 + 5) / 3 = 15 / 3 = 5; ESPERADO: 5
testeA6c = ebigStep (Mult (Sub (Var "x") (Var "y")) (Soma (Var "y") (Num 1)), exArit)   -- (10 - 5) * (5 + 1) = 5 * 6 = 30;  ESPERADO: 30

---------------------------------
---
--- FIM DOS TESTES ARITIMÉTICOS
---
---------------------------------

bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise = True

bbigStep (And b1 b2,s )  = bbigStep (b1,s) && bbigStep (b2,s) -- Processo Direto
-- bbigStep (And b1 b2,s) -- Processo Completo
--  | bbigStep (b1,s) == True    = bbigStep (b2,s)
--  | otherwise                  = False

bbigStep (Or b1 b2,s )  = bbigStep (b1,s) || bbigStep (b2,s) -- Processo Direto
-- bbigStep (Or b1 b2,s ) -- Processo Completo
--  | bbigStep (b1,s) == True   = True
--  | otherwise	= bbigStep (b2,s)

bbigStep (Leq e1 e2,s) = ebigStep (e1,s) <= ebigStep (e2,s) -- Processo Direto
--bbigStep (Leq e1 e2,s) -- Processo Completo
--    | ebigStep (e1,s) <= ebigStep (e2,s)
--    | otherwise	= False

bbigStep (Igual e1 e2,s) = ebigStep (e1,s) == ebigStep (e2,s) -- Processo Direto
-- bbigStep (Igual e1 e2,s) -- Processo Completo
--    | bbigStep (And (Leq e1 e2) (Leq e2 e1), s)

---------------------------------
---
--- COMEÇO DOS TESTES BOOLEANOS
---
---------------------------------

exBool :: Memoria
exBool = [ ("x", 10), ("y",0), ("z",0)]

-- OPERADOR AND
testeB1a = bbigStep (And TRUE FALSE, exBool)                 -- ESPERADO: False
testeB1b = bbigStep (And TRUE TRUE, exBool)                  -- ESPERADO: True
testeB1c = bbigStep (And (Leq (Num 5) (Num 10)) FALSE, exBool) -- ESPERADO: False
testeB1d = bbigStep (And (Leq (Var "x") (Var "y")) (Igual (Var "x") (Var "z")), exBool) -- ESPERADO: False

-- OPERADOR OR
testeB2a = bbigStep (Or TRUE FALSE, exBool)                  -- ESPERADO: True
testeB2b = bbigStep (Or FALSE FALSE, exBool)                 -- ESPERADO: False
testeB2c = bbigStep (Or (Leq (Num 10) (Num 5)) TRUE, exBool)  -- ESPERADO: True
testeB2d = bbigStep (Or (Leq (Var "x") (Var "y")) (Igual (Var "x") (Var "z")), exBool) -- ESPERADO: True

-- OPERADOR LEQ
testeB3a = bbigStep (Leq (Num 5) (Num 10), exBool)             -- ESPERADO: True
testeB3b = bbigStep (Leq (Num 10) (Num 5), exBool)             -- ESPERADO:False
testeB3c = bbigStep (Leq (Var "y") (Var "x"), exBool)          -- ESPERADO: True
testeB3d = bbigStep (Leq (Soma (Var "x") (Num 1)) (Mult (Var "z") (Num 2)), exBool) -- ESPERADO: False

-- OPERADOR IGUAL
testeB4a = bbigStep (Igual (Num 5) (Num 5), exBool)            -- ESPERADO:True
testeB4b = bbigStep (Igual (Num 5) (Num 10), exBool)           -- ESPERADO: False
testeB4c = bbigStep (Igual (Var "y") (Var "z"), exBool)        -- ESPERADO: True
testeB4d = bbigStep (Igual (Soma (Var "x") (Num 0)) (Var "x"), exBool) -- ESPERADO: True
testeB4e = bbigStep (Igual (Soma (Var "x") (Num 1)) (Var "z"), exBool) -- ESPERADO: False

---------------------------------
---
--- FIM DOS TESTES BOOLEANOS
---
---------------------------------

cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s)
   | bbigStep(b,s) == True = cbigStep (c1,s)
   | otherwise = cbigStep (c2,s)
cbigStep (Seq c1 c2,s) = cbigStep (c2,(\ (c,s) -> s) (cbigStep (c1,s))) -- Avalia (c1,s) e retorna a memória do resultado

-- IMPORTANTE
-- A definição de Atrib aqui pede uma memória, mas os exemplos mais adiante são sem essa memória?
cbigStep (Atrib (Var x) e,s) = cbigStep (Skip,(mudaVar s x (ebigStep (e,s))))


--     While B C
--    ThreeTimes C   ---- Executa o comando C 3 vezes
--     DoWhile C B --- DoWhile C B: executa C enquanto B é verdadeiro
--     Loop C E      ---- Loop E C: executa E vezes o comando C 
--     Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
--     ExecWhile E E C -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C 
--     DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop
--- * Dupla Atribuição
--- * Do While
--- * Assert
--- * ExecWhile
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica

---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
