theory RCOrder
imports RC Porder Cont Fun_Cpo
begin

fun myBelow :: "'a::below rc \<Rightarrow> 
                'a rc \<Rightarrow> bool" where
  "myBelow (Leaf x) (Leaf y) = below x y" |
  "myBelow (Leaf x) (Node y l r) = below x y" |
  "myBelow (Node x l r) (Leaf y) = False" |
  "myBelow (Node x l r) (Node y l2 r2) = (below x y \<and> 
                                          myBelow l l2 \<and> 
                                          myBelow r r2)"

instantiation rc :: (below) below
begin
  definition below_rc_def [simp]:
    "(op \<sqsubseteq>) \<equiv> (\<lambda>x y . myBelow x y)"
  instance ..
end

lemma refl_less_rc: "(x::('a::po) rc) \<sqsubseteq> x"
  apply (induction x)
  apply simp
  apply auto
  done

lemma antisym_help: "myBelow (Node x l r) (y::('a::po) rc) \<Longrightarrow> 
                     myBelow y (Node x l r) \<Longrightarrow> 
                    (Node x l r) = y"
  apply (induction y rule:myBelow.induct)
  apply auto
  apply (metis below_antisym)
  apply (metis below_antisym)
  done

lemma antisym_less_rc: "(x::('a::po) rc) \<sqsubseteq> y \<Longrightarrow> 
                        y \<sqsubseteq> x \<Longrightarrow> 
                        x = y"
  apply (induction x)
  apply auto
  apply (induction y)
  apply auto
  apply (metis below_antisym)
  apply (metis antisym_help)
  done

lemma trans_help: "myBelow (Leaf (y::('a::po))) ya \<Longrightarrow> 
                   myBelow ya (Leaf x) \<Longrightarrow> 
                   y \<sqsubseteq> x"
  apply (induction ya)
  apply auto
  apply (metis below_trans)
  done

lemma trans_help2: "(\<And>y. myBelow l2 y \<Longrightarrow> 
                              myBelow y l \<Longrightarrow> 
                              myBelow l2 l) \<Longrightarrow>
                    myBelow (Node y l2 r2) ya \<Longrightarrow> 
                    myBelow ya (Node x l r) \<Longrightarrow> 
                    myBelow l2 l"
  apply (induction ya)
  apply auto
  done

lemma trans_help3: "(\<And>y. myBelow r2 y \<Longrightarrow> 
                              myBelow y r \<Longrightarrow> 
                              myBelow r2 r) \<Longrightarrow>
                    myBelow (Node y l2 r2) ya \<Longrightarrow> 
                    myBelow ya (Node x l r) \<Longrightarrow> 
                    myBelow r2 r"
  apply (induction ya)
  apply auto
  done

lemma trans_less_rc: "(x::('a::po) rc) \<sqsubseteq> y \<Longrightarrow> 
                      y \<sqsubseteq> z \<Longrightarrow> 
                      x \<sqsubseteq> z"
  apply (induction x arbitrary: y rule:myBelow.induct)
  apply auto
  apply (metis trans_help)
  apply (metis myBelow.simps(3) rc.exhaust)
  apply (metis (full_types) below_trans myBelow.simps(1) 
                            myBelow.simps(2) 
                            myBelow.simps(4) 
                            rc.distinct(1) 
                            rc.exhaust)
  apply (metis (full_types) below_refl 
                            box_below 
                            myBelow.simps(3) 
                            myBelow.simps(4) 
                            rc.exhaust)
  apply (metis trans_help2)
  apply (metis trans_help3)
  done

instantiation rc :: (po)po
begin
  instance
  apply intro_classes
  apply (metis refl_less_rc)
  apply (metis trans_less_rc)
  apply (metis antisym_less_rc)
  done
end

fun rc_order :: "('a::po) rc \<Rightarrow> bool" where
  "rc_order (Leaf x) = True" |
  "rc_order (Node x l r) = (x \<sqsubseteq> (getValue l []) \<and> 
                            x \<sqsubseteq> (getValue r []) \<and> 
                            rc_order l \<and> 
                            rc_order r)"

lemma getValue_order_help: "(\<And>xs. getValue l [] \<sqsubseteq> 
                                       getValue l xs) \<Longrightarrow>
                            (\<And>xs. getValue r [] \<sqsubseteq> 
                                       getValue r xs) \<Longrightarrow>
                            x \<sqsubseteq> getValue l [] \<Longrightarrow>
                            x \<sqsubseteq> getValue r [] \<Longrightarrow> 
                            rc_order l \<Longrightarrow> 
                            rc_order r \<Longrightarrow> 
                            x \<sqsubseteq> getValue (Node x l r) ys"
  apply (induction ys)
  apply auto
  apply (metis (full_types) below_refl 
                            box_below 
                            getValue.simps(3) 
                            getValue.simps(4))
  done

lemma getValue_order_help2: "x \<sqsubseteq> getValue t [] \<Longrightarrow> 
                             rc_order t \<Longrightarrow> 
                             x \<sqsubseteq> getValue t ys"
  apply (induction ys rule:getValue.induct)
  apply auto
  apply (metis below_trans)
  apply (metis below_trans)
  done

lemma getValue_order_help3: "rc_order x \<Longrightarrow> 
                             getValue x xs \<sqsubseteq> getValue x (xs @ ys)"
  apply (induction xs arbitrary: ys rule: getValue.induct)
  apply auto  
  apply (induction ys)
  apply auto
  apply (metis (full_types) getValue.simps(2) 
                            getValue_order_help2 
                            po_eq_conv 
                            rc_order.simps(2))
  done

lemma getValue_order_help4: "rc_order xa \<Longrightarrow>
                             myBelow xa (Node x l r) \<Longrightarrow>
                             x \<sqsubseteq> getValue l [] \<Longrightarrow>
                             x \<sqsubseteq> getValue r [] \<Longrightarrow> 
                             rc_order l \<Longrightarrow> 
                             rc_order r \<Longrightarrow> 
                             getValue xa [] \<sqsubseteq> x"
  apply (induction xa)
  apply (metis getValue.simps(1) 
               myBelow.simps(2))
  apply (metis getValue.simps(2) 
               myBelow.simps(4))
  done

lemma getValue_order_help5: "(\<And>x. rc_order x \<Longrightarrow> 
                                       myBelow x l \<Longrightarrow> 
                                       getValue x ys \<sqsubseteq> 
                                       getValue l ys) \<Longrightarrow>
                             rc_order xa \<Longrightarrow>
                             myBelow xa (Node x l r) \<Longrightarrow>
                             x \<sqsubseteq> getValue l [] \<Longrightarrow>
                             x \<sqsubseteq> getValue r [] \<Longrightarrow>
                             rc_order l \<Longrightarrow> 
                             rc_order r \<Longrightarrow> 
                             getValue xa (False # ys) \<sqsubseteq> getValue l ys"
  apply (induction xa)
  apply auto
  apply (metis below_trans getValue_order_help2)
  done

lemma getValue_order_help6: "(\<And>x. rc_order x \<Longrightarrow> 
                                       myBelow x r \<Longrightarrow> 
                                       getValue x ys \<sqsubseteq> 
                                       getValue r ys) \<Longrightarrow>
                             rc_order xa \<Longrightarrow>
                             myBelow xa (Node x l r) \<Longrightarrow>
                             x \<sqsubseteq> getValue l [] \<Longrightarrow>
                             x \<sqsubseteq> getValue r [] \<Longrightarrow>
                             rc_order l \<Longrightarrow> 
                             rc_order r \<Longrightarrow> 
                             getValue xa (True # ys) \<sqsubseteq> getValue r ys"
  apply (induction xa)
  apply auto
  apply (metis below_trans 
               getValue_order_help2)
  done

lemma getValue_order: "rc_order x \<Longrightarrow> 
                       rc_order y \<Longrightarrow> 
                       myBelow x y \<Longrightarrow> 
                       getValue x xs \<sqsubseteq> getValue y xs"
  apply (induction xs arbitrary: x rule:getValue.induct)
  apply auto
  apply (metis getValue.simps(1) 
               myBelow.simps(1) 
               myBelow.simps(3) 
               rc_order.cases)
  apply (metis getValue_order_help4)
  apply (metis getValue_order_help5)
  apply (metis getValue_order_help6)
  done

definition ordered :: "('a::po \<Rightarrow> 'a rc) \<Rightarrow> bool" where
  "ordered f = (\<forall>x.(rc_order (f x)))"

lemma getTree_ordered: "rc_order x \<Longrightarrow> rc_order (getTree x xs)"
  apply (induction x xs rule:getTree.induct)
  apply auto
  done

lemma getTree_order_help: "x \<sqsubseteq> getValue t [] \<Longrightarrow> myBelow (Leaf x) t"
  apply (metis getValue.simps(1) 
               getValue.simps(2) 
               myBelow.simps(1) 
               myBelow.simps(2) 
               rc.exhaust)
  done

lemma getTree_order_help2: "rc_order y \<Longrightarrow> 
                            myBelow (Leaf x) y \<Longrightarrow> 
                            myBelow (Leaf x) (getTree y ys)"
  apply (induction y ys rule:getTree.induct)
  apply auto
  apply (subgoal_tac "myBelow (Leaf x) l")
  apply auto
  apply (metis getTree_order_help 
               rev_below_trans)
  apply (subgoal_tac "myBelow (Leaf x) r")
  apply auto
  apply (metis getTree_order_help 
               rev_below_trans)
  done
  

lemma getTree_order: "rc_order x \<Longrightarrow> 
                      rc_order y \<Longrightarrow> 
                      myBelow x y \<Longrightarrow> 
                      getTree x xs \<sqsubseteq> getTree y xs"
  apply (induction y xs arbitrary: x rule:getTree.induct)
  apply auto
  apply (metis getTree.simps(1) 
               myBelow.simps(3) 
               rc.exhaust)
  apply (case_tac xa)
  apply auto
  apply (metis getTree.simps(1) 
               getTree_order_help 
               rc_order.simps(1) 
               rev_below_trans)
  apply (case_tac xa)
  apply auto
  apply (metis getTree.simps(1) 
               getTree_order_help 
               rc_order.simps(1) 
               rev_below_trans)
  done

lemma kleisli_order_help: "(\<And>xs. rc_order (kleisli f l xs)) \<Longrightarrow>
                            \<forall>x y. x \<sqsubseteq> y \<longrightarrow> 
                                          myBelow (f x) (f y) \<Longrightarrow>
                            \<forall>x. rc_order (f x) \<Longrightarrow>
                            x \<sqsubseteq> getValue l [] \<Longrightarrow>
                            rc_order l \<Longrightarrow>
                            getValue (f x) xs \<sqsubseteq> 
                            getValue (kleisli f l (xs @ [b])) []"
  apply (induction l)
  apply auto
  apply (metis box_below 
               getValTree 
               getValue_order 
               getValue_order_help3)
  apply (subgoal_tac "f x \<sqsubseteq> f a")
  apply (subgoal_tac "getValue (f x) xs \<sqsubseteq> getValue (f a) xs")
  apply (metis below_trans 
               getValue_order_help3)
  apply (metis getValue_order)
  apply auto
  done

lemma kleisli_order: "monofun f \<Longrightarrow> 
                      ordered f \<Longrightarrow> 
                      rc_order x \<Longrightarrow> 
                      rc_order (kleisli f x xs)"
  apply (simp only: monofun_def)
  apply (simp only: ordered_def)
  apply auto
  apply (induction x arbitrary: xs rule:rc_order.induct)
  apply auto
  apply (metis getTree_ordered)
  apply (metis kleisli_order_help)
  apply (metis kleisli_order_help)
  done  

lemma "monofun f \<Longrightarrow> 
       ordered f \<Longrightarrow> 
       rc_order x \<Longrightarrow> 
       rc_order (flatMap f x)"
  apply (metis flatMap.simps 
               kleisli_order)
  done  

lemma order_myBelow: "x \<sqsubseteq> y \<Longrightarrow> myBelow x y"
  apply auto
  done

lemma "rc_order (f x) \<Longrightarrow> 
       rc_order (f a) \<Longrightarrow> 
       myBelow (f x) (f a) \<Longrightarrow> 
       myBelow (getTree (f x) ys) (getTree (f a) ys)"
  apply (metis (hide_lams, mono_tags) 
               getTree_order 
               order_myBelow)
  done

lemma myBelow_alt: "getValue x [] \<sqsubseteq> y \<Longrightarrow> 
                    getTree x [False] \<sqsubseteq> l \<Longrightarrow> 
                    getTree x [True] \<sqsubseteq> r \<Longrightarrow> 
                    myBelow x (Node y l r)"
  apply auto
  apply (induction x)
  apply auto
  done

lemma kleisli_monotone_help: "(\<And>x. rc_order x \<Longrightarrow>
                                        myBelow x l \<Longrightarrow>
                                        myBelow (kleisli f x (word @ [False])) 
                                                (kleisli f l (word @ [False]))) \<Longrightarrow>
                              (\<And>x. rc_order x \<Longrightarrow>
                                        myBelow x r \<Longrightarrow>
                                        myBelow (kleisli f x (word @ [True])) 
                                                (kleisli f r (word @ [True]))) \<Longrightarrow>
                              \<forall>x y. x \<sqsubseteq> y \<longrightarrow> 
                                            myBelow (f x) (f y) \<Longrightarrow>
                              \<forall>x. rc_order (f x) \<Longrightarrow>
                              rc_order xa \<Longrightarrow>
                              myBelow xa (Node x l r) \<Longrightarrow>
                              x \<sqsubseteq> getValue l [] \<Longrightarrow>
                              x \<sqsubseteq> getValue r [] \<Longrightarrow>
                              rc_order l \<Longrightarrow>
                              rc_order r \<Longrightarrow>
                              myBelow (kleisli f xa word)
                                      (Node (getValue (f x) word) 
                                      (kleisli f l (word @ [False]))
                                      (kleisli f r (word @ [True])))"
  apply (induction xa)
  apply auto
  apply (rule myBelow_alt)
  apply auto
  apply (metis getValKleisli 
               getValue.simps(1) 
               getValue.simps(2) 
               getValue_order 
               kleisli.simps(1) 
               kleisli.simps(2))
  apply (metis (hide_lams, no_types) 
               below_trans 
               getTree_order_help 
               kleisli.simps(1) 
               po_eq_conv 
               rc_order.simps(1))
  apply (metis (hide_lams, no_types) 
               below_trans 
               getTree_order_help 
               kleisli.simps(1) 
               po_eq_conv 
               rc_order.simps(1))
  apply (metis getValue_order)
  done

lemma kleisli_monotone: "\<forall>x y. x \<sqsubseteq> y \<longrightarrow> 
                                       myBelow (f x) (f y) \<Longrightarrow>
                         \<forall>x. rc_order (f x) \<Longrightarrow>
                         rc_order x \<Longrightarrow>
                         rc_order y \<Longrightarrow> 
                         myBelow x y \<Longrightarrow> 
                         myBelow (kleisli f x xs) (kleisli f y xs)"
  apply (induction f y xs arbitrary: x rule:kleisli.induct)
  apply auto
  apply (case_tac xa)
  apply auto
  apply (metis getTree_order 
               order_myBelow)
  apply (metis kleisli_monotone_help)
  done  
  
lemma "monofun f \<Longrightarrow> 
       ordered f \<Longrightarrow> 
       rc_order x \<Longrightarrow> 
       rc_order y \<Longrightarrow> 
       x \<sqsubseteq> y \<Longrightarrow> 
       (flatMap f x) \<sqsubseteq> (flatMap f y)"
  apply (simp only: monofun_def)
  apply (simp only: ordered_def)
  apply auto
  by (metis kleisli_monotone)
