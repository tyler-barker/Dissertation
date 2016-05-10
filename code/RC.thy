theory RC
imports Main
begin

datatype 'a rc = 
  Leaf 'a |
  Node 'a "'a rc" "'a rc"

fun getValue :: "'a rc \<Rightarrow> bool list \<Rightarrow> 'a" where
  "getValue (Leaf x) ys  = x" |
  "getValue (Node x l r) [] = x" |
  "getValue (Node x l r) (False # ys) = getValue l ys" |
  "getValue (Node x l r) (True # ys) = getValue r ys"

fun getTree :: "'a rc \<Rightarrow> bool list \<Rightarrow> 'a rc" where
  "getTree (Leaf x) ys = (Leaf x)" |
  "getTree r [] = r" |
  "getTree (Node x l r) (False # ys) = getTree l ys" |
  "getTree (Node x l r) (True # ys) = getTree r ys"

lemma [simp]: "getTree t [] = t"
  apply (induct t)
  apply auto
  done

fun fmap :: "('a => 'b) => 'a rc => 'b rc" where
  "fmap f (Leaf x) = Leaf (f x)" |
  "fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)"

fun unit :: "'a \<Rightarrow> 'a rc" where
  "unit x = Leaf x"

fun kleisli :: "('a \<Rightarrow> 'b rc) \<Rightarrow> 
                 'a rc \<Rightarrow> 
                 bool list \<Rightarrow> 
                 'b rc" where
  "kleisli f (Leaf x) word = getTree (f x) word" |
  "kleisli f (Node x l r) word = Node (getValue (f x) word) 
                                      (kleisli f l (word @ [False])) 
                                      (kleisli f r (word @ [True]))" 

fun flatMap :: "('a \<Rightarrow> 'b rc) \<Rightarrow> 
                 'a rc \<Rightarrow> 
                 'b rc" where
  "flatMap f x = kleisli f x []"   

fun inTree :: "'a rc \<Rightarrow> 
               bool list \<Rightarrow> 
               bool" where
  "inTree t [] = True" |
  "inTree (Leaf x) ys = False" |
  "inTree (Node x l r) (False # ys) = inTree l ys" |
  "inTree (Node x l r) (True # ys) = inTree r ys"

lemma [simp]: "getTree t ys = Leaf a \<Longrightarrow> 
               getTree t (ys @ b) = Leaf a"
  apply (induct t ys rule: getTree.induct)
  apply simp_all
  done

lemma "flatMap f (unit x) = f x"
  by simp


lemma [simp]: "kleisli unit t ys = t"
  apply (induction t arbitrary: ys)
  apply auto
  done

lemma "(flatMap unit) x = x"
  by auto
  
lemma [simp]: "getValue (k (getValue t [])) [] = 
               getValue (kleisli k t []) []"
  apply (induct t)
  apply auto
  done

lemma "getTree (Node a l r) (b # ys) = 
       getTree (getTree (Node a l r) [b]) ys"
  apply (induct b)
  apply auto
  done

lemma tree: "getTree t (xs @ ys) = getTree (getTree t xs) ys"
  apply (induction t xs rule:getTree.induct)
  apply auto
  done

lemma [simp]: "getTree (getTree t xs) ys = getTree t (xs @ ys)"
  by (metis tree)

lemma tree2: "getTree t (x # ys) = getTree (getTree t [x]) ys"
  apply simp
  done

lemma "getValue ((flatMap k) ((flatMap h) t)) [] = 
       getValue ((flatMap (\<lambda>x. (flatMap k) (h x))) t) []"
  apply auto
  apply (induct t)
  apply auto
  done

lemma list: "xs @ a # ys = (xs @ [a]) @ ys"
  by auto

lemma kleisli: "kleisli k (getTree t ys) (xs @ ys) = 
                getTree (kleisli k t xs) ys"
  apply (induct t ys arbitrary: xs rule:getTree.induct )
  apply auto
  apply (metis list)
  apply (metis list)
  done

lemma getValTree: "getValue t (xs @ ys) = 
                   getValue (getTree t xs) ys"
  apply (induct t xs rule: getTree.induct)
  apply auto
  done

lemma getValKleisli: "getValue (k (getValue t ys)) (xs @ ys) = 
                      getValue (kleisli k t xs) ys"
  apply (induct t ys arbitrary: xs rule:getValue.induct)
  apply auto
  apply (metis getValTree)
  apply (metis append_Cons append_Nil append_assoc)
  apply (metis append_Cons append_Nil append_assoc)
  done

lemma kleisli2: "kleisli k (kleisli h t ys) ys = 
                 kleisli (\<lambda>x. kleisli k (h x) []) t ys"
  apply (induct t arbitrary: ys)
  apply auto 
  apply (metis append_Nil kleisli)
  apply (metis append_Nil getValKleisli)
  done

lemma "(flatMap k) ((flatMap h) t) = 
       (flatMap (\<lambda>x. (flatMap k) (h x))) t"
  apply auto
  apply (induction t)
  apply auto
  apply (metis kleisli2)
  apply (metis kleisli2)
  done

end
