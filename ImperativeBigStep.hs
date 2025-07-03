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

---------------------------------
---
--- COMEÇO DAS FUNÇÕES ARITIMÉTICAS
---
---------------------------------

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)
  | ebigStep (e1,s) - ebigStep (e2,s) < 0 = error ("Resultado não é um número natural")
  | otherwise = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep(Div e1 e2,s) = ebigStep (e1,s) `div` ebigStep (e2,s)

---------------------------------
---
--- FIM DAS FUNÇÕES ARITIMÉTICAS
---
---------------------------------
---------------------------------
---
--- COMEÇO DOS TESTES ARITIMÉTICOS
---
---------------------------------

exArit :: Memoria
exArit = [ ("x", 10), ("y",5), ("z",0)]

-- Os valores esperados abaixo são utilizando a memória de exemplo exArit
-- NUMEROS E VARIÁVEIS
testeA1a :: E
testeA1a = (Num 15)                         -- ESPERADO: 15

testeA1b :: E
testeA1b = (Var "x")                        -- ESPERADO: 10

testeA1c :: E 
testeA1c = (Var "y")                        -- ESPERADO: 5

-- SOMAS
testeA2a :: E
testeA2a = (Soma (Num 5) (Num 3))           -- 5 + 3 = 8; ESPERADO: 08

testeA2b :: E
testeA2b = (Soma (Var "x") (Num 2))         -- 10 + 2 = 12; ESPERADO: 12

testeA2c :: E
testeA2c = (Soma (Var "x") (Var "y"))       -- 10 + 5 = 15; ESPERADO: 15

-- SUBTRAÇÕES
testeA3a :: E
testeA3a = (Sub (Num 10) (Num 4))           -- 10 - 4 = 6; ESPERADO: 6

testeA3b :: E
testeA3b = (Sub (Var "x") (Num 5))          -- 10 - 5 = 5; ESPERADO: 5

-- MULTIPLICAÇÕES
testeA4a :: E
testeA4a = (Mult (Num 3) (Num 6))           -- 3 * 6 = 18; ESPERADO: 18

testeA4b :: E
testeA4b = (Mult (Var "y") (Num 2))         -- 5 * 2 = 10; ESPERADO: 10

testeA4c :: E
testeA4c = (Mult (Var "x") (Var "y"))       -- 10 * 5 = 50; ESPERADO: 50

-- DIVISÕES
testeA5a = (Div (Num 10) (Num 2))           -- 10 / 2 = 5; ESPERADO: 5
testeA5b = (Div (Var "x") (Num 3))          -- 10 / 3 = 3; ESPERADO: 3 (Divisão sem decimal)
testeA5c = (Div (Var "x") (Var "y"))        -- 10 / 5 = 2; ESPERADO: 2
testeA5d = (Div (Num 10) (Num 0))           -- ESPERADO: "Exception: divide by zero" 

-- EXPRESSÕES COMPLEXAS (MULTIPLAS CONTAS)
testeA6a = (Soma (Mult (Num 2) (Num 3)) (Sub (Num 10) (Num 5)))        -- (2 * 3) + (10 - 5) = 6 + 5 = 11; ESPERADO: 11
testeA6b = (Div (Soma (Var "x") (Var "y")) (Num 3))                    -- (10 + 5) / 3 = 15 / 3 = 5; ESPERADO: 5
testeA6c = (Mult (Sub (Var "x") (Var "y")) (Soma (Var "y") (Num 1)))   -- (10 - 5) * (5 + 1) = 5 * 6 = 30;  ESPERADO: 30

---------------------------------
---
--- FIM DOS TESTES ARITIMÉTICOS
---
---------------------------------

---------------------------------
---
--- COMEÇO DAS FUNÇÕES BOOLEANAS
---
---------------------------------

bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
 | bbigStep (b,s) == True = False
 | otherwise = True

--bbigStep (And b1 b2,s )  = bbigStep (b1,s) && bbigStep (b2,s) -- Processo Direto
bbigStep (And b1 b2,s) -- Processo Completo
 | bbigStep (b1,s) == True = bbigStep (b2,s)
 | otherwise = False

--bbigStep (Or b1 b2,s )  = bbigStep (b1,s) || bbigStep (b2,s) -- Processo Direto
bbigStep (Or b1 b2,s ) -- Processo Completo
 | bbigStep (b1,s) == True = True
 | otherwise = bbigStep (b2,s)

-- bbigStep (Leq e1 e2,s) = ebigStep (e1,s) <= ebigStep (e2,s) -- Processo Direto
bbigStep (Leq e1 e2,s) -- Processo Completo
 | ebigStep (e1,s) <= ebigStep (e2,s) = True
 | otherwise = False

-- bbigStep (Igual e1 e2,s) = ebigStep (e1,s) == ebigStep (e2,s) -- Processo Direto
bbigStep (Igual e1 e2,s) = bbigStep (And (Leq e1 e2) (Leq e2 e1), s) -- Processo Completo 

---------------------------------
---
--- FIM DAS FUNÇÕES BOOLEANAS
---
---------------------------------

---------------------------------
---
--- COMEÇO DOS TESTES BOOLEANOS
---
---------------------------------

exBool :: Memoria
exBool = [ ("x", 10), ("y",0), ("z",0)]

-- OPERADOR AND
-- Os resultados esperados são utilizando a memória "exBool"
testeB1a :: B
testeB1a = (And TRUE FALSE)                   -- ESPERADO: False

testeB1b :: B
testeB1b = (And TRUE TRUE)                    -- ESPERADO: True

testeB1c :: B
testeB1c = (And (Leq (Num 5) (Num 10)) FALSE) -- ESPERADO: False

testeB1d :: B
testeB1d = (And (Leq (Var "x") (Var "y")) (Igual (Var "x") (Var "z"))) -- ESPERADO: False

-- OPERADOR OR
testeB2a :: B
testeB2a = (Or TRUE FALSE)                   -- ESPERADO: True

testeB2b :: B
testeB2b = (Or FALSE FALSE)                  -- ESPERADO: False

testeB2c :: B
testeB2c = (Or (Leq (Num 10) (Num 5)) TRUE)  -- ESPERADO: True

testeB2d :: B
testeB2d = (Or (Leq (Var "x") (Var "y")) (Igual (Var "x") (Var "z"))) -- ESPERADO: True

-- OPERADOR LEQ
testeB3a :: B
testeB3a = (Leq (Num 5) (Num 10))             -- ESPERADO: True

testeB3b :: B
testeB3b = (Leq (Num 10) (Num 5))             -- ESPERADO:False

testeB3c :: B
testeB3c = (Leq (Var "y") (Var "x"))          -- ESPERADO: True

testeB3d :: B
testeB3d = (Leq (Soma (Var "x") (Num 1)) (Mult (Var "z") (Num 2))) -- ESPERADO: False

-- OPERADOR IGUAL
testeB4a :: B
testeB4a = (Igual (Num 5) (Num 5))            -- ESPERADO:True

testeB4b :: B
testeB4b = (Igual (Num 5) (Num 10))           -- ESPERADO: False

testeB4c :: B
testeB4c = (Igual (Var "y") (Var "z"))        -- ESPERADO: True

testeB4d :: B
testeB4d = (Igual (Soma (Var "x") (Num 0)) (Var "x")) -- ESPERADO: True

testeB4e :: B
testeB4e = (Igual (Soma (Var "x") (Num 1)) (Var "z")) -- ESPERADO: False

---------------------------------
---
--- FIM DOS TESTES BOOLEANOS
---
---------------------------------

---------------------------------
---
--- COMEÇO DAS FUNÇÕES CONDICIONAIS
---
---------------------------------

cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s)
 | bbigStep(b,s) == True = cbigStep (c1,s)
 | otherwise = cbigStep (c2,s)
cbigStep (Seq c1 c2,s) = cbigStep (c2, snd (cbigStep (c1,s)))
-- Snd é uma função que retorna o segundo valor de uma tupla, neste caso como definido (C, Memoria), ele retorna a memória após executar c1
cbigStep (Atrib (Var x) e,s) = cbigStep (Skip,(mudaVar s x (ebigStep (e,s))))

--    While B C
cbigStep (While b c, s)
 | bbigStep (b,s) == True = cbigStep (Seq c (While b c), s)
 | otherwise = (Skip, s)

--    ThreeTimes C   ---- Executa o comando C 3 vezes
cbigStep (ThreeTimes c, s) = cbigStep (Seq c (Seq c c), s)

--     DoWhile C B --- DoWhile C B: executa C enquanto B é verdadeiro
cbigStep (DoWhile c b, s) = cbigStep (Seq c (While b c), s)

--     Loop C E      ---- Loop E C: executa E vezes o comando C
-- cbigStep (Loop e c, s) = cbigStep (If (Igual e (Num 0), s) (Skip,s) (Seq C (Loop (Sub (e (Num 1), s) c, s), s), s)
cbigStep (Loop c e, s)
 | ebigStep (e,s) <= 0 = (Skip, s)
 | otherwise = cbigStep (Seq c (Loop c (Sub e (Num 1))), s)

--     Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C (Não é só um If sem else??)
cbigStep (Assert b c,s)
 | bbigStep(b,s) == True = cbigStep (c, s)
 | otherwise = (Skip,s)

--     ExecWhile E E C -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C
cbigStep (ExecWhile e1 e2 c, s)
  | ebigStep(e1,s) < ebigStep(e2,s) = cbigStep (ExecWhile e1 e2 c, snd (cbigStep (c,s)))
  | otherwise = (Skip,s)

-- DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
cbigStep (DAtrrib (Var e1) (Var e2) e3 e4,s) = cbigStep (Seq (Atrib (Var e1) e3)
                  (Atrib (Var e2) e4),s)

---------------------------------
---
--- FIM DAS FUNÇÕES CONDICIONAIS
---
---------------------------------

---------------------------------
---
--- COMEÇO DOS TESTES CONDICIONAIS
---
---------------------------------

exCond :: Memoria
exCond = [("x", 5), ("y", 1), ("z", 0)]

exCond2 :: Memoria
exCond2 = [("x", 7), ("y", 3), ("z", 9)]

-- Os resultados esperados utilizam a memória "exCond"
-- SKIP (Quando sozinho mostra a memória)
testeC1a :: C
testeC1a = Skip   -- ESPERADO: (Skip, [("x",5),("y",1),("z",0)])

-- ATRIBUIÇÃO
testeC2a :: C
testeC2a = (Atrib (Var "x") (Num 10))   -- ESPERADO: (Skip, [("x",10),("y",1),("z",0))])

testeC2b :: C
testeC2b = (Atrib (Var "y") (Soma (Var "x") (Num 2)))   -- ESPERADO: (Skip, [("x",5),("y",7),("z",0)])

-- SEQUÊNCIA
testeC3a :: C
testeC3a = (Seq (Atrib (Var "x") (Num 10)) (Atrib (Var "y") (Soma (Var "x") (Num 5))))
-- x := 10, (Memória: [("x",10),("y",1),("z",0)]);	y := x + 5 = 10 + 5 = 15, (Memória: [("x",10),("y",15),("z",0)]);
-- ESPERADO: (Skip, [("x",10),("y",15),("z",0)])

testeC3b :: C
testeC3b = (Seq (Atrib (Var "z") (Var "x")) (Seq (Atrib (Var "x") (Var "y")) (Atrib (Var "y") (Var "z"))))
-- z := x(5), [("x",5),("y",1),("z",5)];	x := y(1), [("x",1),("y",1),("z",5)];		y := z(5), [("x",1),("y",5),("z",5)]
-- ESPERADO: (Skip, [("x",1),("y",5),("z",5)])

-- SE/ENTÃO
testeC4a :: C
testeC4a = (If (Leq (Num 5) (Num 10))(Atrib (Var "x") (Num 100))(Atrib (Var "x") (Num 0)))
-- 5 <= 10 é TRUE, então x := 100.	ESPERADO: (Skip, [("x",100),("y",1),("z",0)])

testeC4b :: C
testeC4b = (If (Leq (Num 10) (Num 5))(Atrib (Var "x") (Num 100))(Atrib (Var "x") (Num 0)))
-- 10 <= 5 é FALSE, então x := 0.		ESPERADO: (Skip, [("x",0),("y",1),("z",0)])

testeC4c :: C
testeC4c = (If (Igual (Var "x") (Num 5))(Atrib (Var "z") (Num 10))(Atrib (Var "z") (Num 5)))
-- x (5) == 5 é TRUE, então z := 10.	ESPERADO: (Skip, [("x",5),("y",1),("z",10)])

-- ENQUANTO
testeC5a :: C
testeC5a = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 0)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
-- Enquanto x for diferente de 0 faz y := y * x e x := x-1
-- (x=5,y=1) -> x!=0? T -> y=1*5=5, x=4;		(x=4,y=5) -> x!=0? T -> y=5*4=20, x=3;		(x=3,y=20) -> x!=0? T -> y=20*3=60, x=2
-- (x=2,y=60) -> x!=0? T -> y=60*2=120, x=1;	(x=1,y=120) -> x!=0? T -> y=120*1=120, x=0;		(x=0,y=120) -> x!=0? F -> FIM
-- ESPERADO: (Skip, [("x",0),("y",120),("z",0)])

-- TRÊS VEZES
testeC6a :: C
testeC6a = (ThreeTimes (Atrib (Var "y") (Soma (Var "y") (Num 1))))
-- Faz y := y+1 três vezes.	ESPERADO: (Skip, [("x",5),("y",4),("z",0)])

-- FAÇA ENQUANTO
testeC7a :: C
testeC7a = (DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                    (Leq (Var "x") (Num 7)))
-- Faz x := x+1 enquanto x <= 7	ESPERADO: (Skip, [("x",8),("y",1),("z",0)])

testeC7b :: C
testeC7b = (DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                    (Leq (Var "x") (Num 0)))
-- Faz x := x+1 enquanto x <= 0	ESPERADO: (Skip, [("x",6),("y",1),("z",0)])

-- LOOP
testeC8a :: C
testeC8a = (Loop (Atrib (Var "y") (Soma (Var "y") (Num 1))) (Num 5))
-- Faz y := y+1, 5 vezes	ESPERADO: (Skip, [("x",5),("y",6),("z",0)])

testeC8b :: C
testeC8b = (Loop (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Num 0))
-- Faz y := y+1, 0 vezes	ESPERADO: (Skip, [("x",5),("y",1),("z",0)])

-- ASSERT
testeC9a :: C
testeC9a = (Assert (Leq (Var "x") (Num 10)) (Atrib (Var "z") (Num 1)))
-- x (5) <= 10 é TRUE, então z := 1	ESPERADO: (Skip, [("x",5),("y",1),("z",1)])

testeC9b :: C
testeC9b = (Assert (Leq (Var "x") (Num 0)) (Atrib (Var "z") (Num 1)))
-- x (5) <= 0 é FALSE, então Skip	ESPERADO: (Skip, [("x",5),("y",1),("z",0)])

-- EXECUTAR ENQUANTO
testeC10a :: C
testeC10a = (ExecWhile (Var "x") (Num 8)(Atrib (Var "x") (Soma (Var "x") (Num 1))))
-- Memória inicial: x=5;	x=5 < 8	TRUE -> x=6;	x=6 < 8 	TRUE -> x=7
-- x=7 < 8 	TRUE -> x=8;	x=8 < 8 	FALSE -> Termina;	ESPERADO: (Skip, [("x",8),("y",1),("z",0)])

testeC10b :: C
testeC10b= (ExecWhile (Var "x") (Num 3) (Atrib (Var "x") (Soma (Var "x") (Num 1))))
-- Memória inicial: x=5;	x=5 < 3 	FALSE -> Termina	ESPERADO: (Skip, [("x",5),("y",1),("z",0)])

testeC10c :: C
testeC10c = (ExecWhile (Var "y") (Num 3) (Atrib (Var "y") (Soma (Var "y") (Num 1))))
-- Memória inicial: y=1;	y=1 < 3 	TRUE -> y=2;	y=2 < 3	TRUE -> y=3;
-- y=3<3 	FALSE -> Termina;	ESPERADO: (Skip, [("x",5),("y",3),("z",0)])

testeC11a :: C
-- DUPLA ATRIBUIÇÃO
testeC11a = (DAtrrib (Var "x") (Var "y") (Num 20) (Num 30))
-- x := 20, y := 30;	ESPERADO: (Skip, [("x",20),("y",30),("z",0)])

testeC11b :: C
testeC11b = (DAtrrib (Var "x") (Var "y") (Soma (Var "x") (Num 1)) (Soma (Var "y") (Num 1)))
-- Memória inicial: x=5, y=1;	x = 5 + 1 = 6;	y = 1 + 1 = 2	x := 6, y := 2;
-- ESPERADO: (Skip, [("x",6),("y",2),("z",0)])

testeC11c :: C
testeC11c = (DAtrrib (Var "x") (Var "y") (Var "y") (Var "x"))
-- Memória inicial: x=5, y=1;	x = y (1) = 1;	y = x (1) = 1;	x := 1, y := 1;
-- ESPERADO: (Skip, [("x",1),("y",1),("z",0)])

testeC11d :: C
testeC11d = (DAtrrib (Var "x") (Var "z") (Mult (Var "x") (Num 2)) (Soma (Var "y") (Var "x")))
-- Memória inicial: x=5, y=1, z=0;		x = x * 2 = 5 * 2 = 10;	z = y + x = 1 + 10 = 11;
-- Esperado: (Skip, [("x",10),("y",1),("z",11)])

---------------------------------
---
--- FIM DOS TESTES CONDICIONAIS
---
---------------------------------

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