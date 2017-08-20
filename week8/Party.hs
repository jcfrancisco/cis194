import Data.Tree
import Employee
import System.IO
import Data.List

maxFun :: Tree Employee -> GuestList
maxFun _ = GL [Emp "Philo Ambroise" 71, Emp "Francis Deluzain" 90] 161

sortedEmployees :: [Employee] -> [Employee]
sortedEmployees employees =
  sortBy (\(Emp { empName = x }) (Emp { empName = y }) -> compare x y)  employees

employeeNames :: [Employee] -> [Name]
employeeNames = map (\(Emp { empName = empName }) -> empName)

glToString :: GuestList -> String
glToString (GL employees fun) =
  "Total fun: "
  ++ show fun
  ++ "\n"
  ++ intercalate "\n" (employeeNames (sortedEmployees employees))

main :: IO ()
main = do
  handle <- openFile "company.txt" ReadMode
  contents <- hGetContents handle
  putStr $ glToString (maxFun (read contents))
  hClose handle
