use std::collections::HashMap;
use std::fmt::{Result as FmtResult, Write};

use crate::derive::{DerivationTree, Judgement as JudgementTrait, Result};
use crate::print::TokenBuffer;
use crate::systems::common::env::{Env, NamedEnv, TypedEnv};
use crate::systems::common::judgement::{
    EvalToJudgement, Judgement, LtIsJudgement, MinusIsJudgement, PlusIsJudgement, TimesIsJudgement,
    TypeJudgement,
};
use crate::systems::common::syntax::{
    ApplicationNode, AsListSeg, AsOpNums, AsParam, AstRoot, BooleanNode, FunctionNode, IfNode,
    IntegerNode, LetInNode, LetRecInNode, ListConcatNode, ListPatternMatchNode, NilListNode, Op,
    OpNode, VariableNode,
};
use crate::systems::common::ty::{MonoType, PolyType};
use crate::systems::common::value::{ConcatList, Function, RecursiveFunction, Value};
use crate::visitor::{Visitable, Visitor};
use alphabet::*;

alphabet!(ENGLISH = "abcdefghijklmnopqrstuvwxyz");

pub struct PrintVisitor {
    pub buffer: TokenBuffer,
    pub ty_var_map: HashMap<usize, String>,
    pub ident_iter: <[char] as Alphabet<'static>>::IterWord,
}

impl PrintVisitor {
    pub fn sub_visitor(&mut self) -> Self {
        let mut ident_iter = ENGLISH.iter_words();
        ident_iter.next();
        PrintVisitor {
            buffer: self.buffer.sub_buffer(),
            ty_var_map: HashMap::new(),
            ident_iter,
        }
    }
    pub fn parenthesized_visit<T: Visitable>(&mut self, node: &T) -> FmtResult
    where
        Self: Visitor<T, FmtResult>,
    {
        self.buffer.write_char('(')?;
        self.visit(node)?;
        self.buffer.write_char(')')
    }

    pub fn new(indent: usize) -> Self {
        let mut ident_iter = ENGLISH.iter_words();
        ident_iter.next();
        Self {
            buffer: TokenBuffer::new(indent),
            ty_var_map: HashMap::new(),
            ident_iter,
        }
    }

    pub fn with_ty_var_map(&mut self, ty_var_map: HashMap<usize, String>) -> Self {
        let mut ident_iter = ENGLISH.iter_words();
        ident_iter.next();
        PrintVisitor {
            buffer: self.buffer.sub_buffer(),
            ty_var_map,
            ident_iter,
        }
    }

    pub fn visit_by_iter<I>(
        &mut self,
        iter: impl IntoIterator<Item = I>,
        mut op: impl FnMut(I, &mut Self) -> FmtResult,
        join: impl Fn(&mut Self) -> FmtResult,
    ) -> FmtResult {
        iter.into_iter()
            .enumerate()
            .fold(Ok(self), |c, (index, item)| {
                if index == 0 {
                    c
                } else {
                    c.and_then(|b| join(b).map(|_| b))
                }
                .and_then(|b| op(item, b).map(|_| b))
            })
            .map(|_| ())
    }
}
impl From<PrintVisitor> for Result<TokenBuffer> {
    fn from(mut v: PrintVisitor) -> Self {
        v.buffer.commit_line(false)?;
        Ok(v.buffer)
    }
}

impl Visitor<IntegerNode, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &IntegerNode) -> FmtResult {
        write!(self.buffer, "{}", node.0)
    }
}

impl Visitor<BooleanNode, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &BooleanNode) -> FmtResult {
        write!(self.buffer, "{}", node.0)
    }
}

impl Visitor<VariableNode, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &VariableNode) -> FmtResult {
        self.buffer.write_str(node.0.as_str())
    }
}

impl Visitor<Op, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &Op) -> FmtResult {
        self.buffer.write_str(match node {
            Op::Plus => "+",
            Op::Minus => "-",
            Op::Times => "*",
            Op::Lt => "<",
        })
    }
}

impl<Ast: AstRoot + AsOpNums> Visitor<OpNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &OpNode<Ast>) -> FmtResult {
        if node.lhs.need_paren(node.op, true) {
            self.parenthesized_visit(node.lhs.as_ref())?;
        } else {
            node.lhs.apply_visitor(self)?;
        }

        node.op.apply_visitor(self)?;
        if node.rhs.need_paren(node.op, false) {
            self.parenthesized_visit(node.rhs.as_ref())
        } else {
            node.rhs.apply_visitor(self)
        }
    }
}

impl<Ast: AstRoot> Visitor<IfNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &IfNode<Ast>) -> FmtResult {
        self.buffer.write_str("if")?;
        node.cond.apply_visitor(self)?;
        self.buffer.write_str("then")?;
        node.t_branch.apply_visitor(self)?;
        self.buffer.write_str("else")?;
        node.f_branch.apply_visitor(self)
    }
}

impl<Ast: AstRoot> Visitor<LetInNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &LetInNode<Ast>) -> FmtResult {
        self.buffer.write_str("let")?;
        self.buffer.write_str(node.ident.as_str())?;
        self.buffer.write_char('=')?;
        node.expr_1.apply_visitor(self)?;
        self.buffer.write_str("in")?;
        node.expr_2.apply_visitor(self)
    }
}

impl<Ast: AstRoot> Visitor<FunctionNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &FunctionNode<Ast>) -> FmtResult {
        self.buffer.write_str("fun")?;
        self.buffer.write_str(node.bind.as_str())?;
        self.buffer.write_str("->")?;
        node.body.apply_visitor(self)
    }
}

impl<Ast: AstRoot> Visitor<LetRecInNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &LetRecInNode<Ast>) -> FmtResult {
        self.buffer.write_str("let rec")?;
        self.buffer.write_str(node.ident.as_str())?;
        self.buffer.write_str("=")?;
        self.buffer.write_str("fun")?;
        self.buffer.write_str(node.bind.as_str())?;
        self.buffer.write_str("->")?;
        node.body.apply_visitor(self)?;
        self.buffer.write_str("in")?;
        node.expr.apply_visitor(self)
    }
}

impl<Ast: AstRoot + AsParam> Visitor<ApplicationNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &ApplicationNode<Ast>) -> FmtResult {
        node.f.apply_visitor(self)?;
        if node.p.need_paren() {
            self.parenthesized_visit(node.p.as_ref())
        } else {
            node.p.apply_visitor(self)
        }
    }
}

impl Visitor<NilListNode, FmtResult> for PrintVisitor {
    fn visit(&mut self, _node: &NilListNode) -> FmtResult {
        self.buffer.write_str("[]")
    }
}

impl<Ast: AstRoot + AsListSeg> Visitor<ListConcatNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &ListConcatNode<Ast>) -> FmtResult {
        if node.lhs.need_paren(true) {
            self.parenthesized_visit(node.lhs.as_ref())?;
        } else {
            node.lhs.apply_visitor(self)?;
        }

        self.buffer.write_str("::")?;
        if node.rhs.need_paren(false) {
            self.parenthesized_visit(node.rhs.as_ref())
        } else {
            node.rhs.apply_visitor(self)
        }
    }
}

impl<E: Env> Visitor<Judgement<E>, FmtResult> for PrintVisitor
where
    Self: Visitor<E::Ast, FmtResult>
        + Visitor<EvalToJudgement<E>, FmtResult>
        + Visitor<PlusIsJudgement, FmtResult>,
{
    fn visit(&mut self, node: &Judgement<E>) -> FmtResult {
        match node {
            Judgement::EvalTo(eval_to) => eval_to.apply_visitor(self),
            Judgement::PlusIs(plus_is) => plus_is.apply_visitor(self),
            Judgement::MinusIs(minus_is) => minus_is.apply_visitor(self),
            Judgement::TimesIs(times_is) => times_is.apply_visitor(self),
            Judgement::LtIs(lt_is) => lt_is.apply_visitor(self),
        }
    }
}

impl<E: Env> Visitor<EvalToJudgement<E>, FmtResult> for PrintVisitor
where
    Self: Visitor<E::Ast, FmtResult> + Visitor<E, FmtResult> + Visitor<Value<E>, FmtResult>,
{
    fn visit(&mut self, node: &EvalToJudgement<E>) -> FmtResult {
        node.env.apply_visitor(self)?;
        self.buffer.write_str("|-")?;
        node.term.apply_visitor(self)?;
        self.buffer.write_str("evalto")?;
        node.value.apply_visitor(self)
    }
}

impl Visitor<PlusIsJudgement, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &PlusIsJudgement) -> FmtResult {
        write!(self.buffer, "{} plus {} is {}", node.0, node.1, node.2)
    }
}

impl Visitor<MinusIsJudgement, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &MinusIsJudgement) -> FmtResult {
        write!(self.buffer, "{} minus {} is {}", node.0, node.1, node.2)
    }
}

impl Visitor<TimesIsJudgement, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &TimesIsJudgement) -> FmtResult {
        write!(self.buffer, "{} times {} is {}", node.0, node.1, node.2)
    }
}

impl Visitor<LtIsJudgement, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &LtIsJudgement) -> FmtResult {
        write!(self.buffer, "{} less than {} is {}", node.0, node.1, node.2)
    }
}

impl<E: Env> Visitor<Value<E>, FmtResult> for PrintVisitor
where
    Self: Visitor<E::Ast, FmtResult> + Visitor<E, FmtResult>,
{
    fn visit(&mut self, node: &Value<E>) -> FmtResult {
        match node {
            Value::Integer(i) => write!(self.buffer, "{}", i),
            Value::Boolean(b) => write!(self.buffer, "{}", b),
            Value::Fun(f) => f.apply_visitor(self),
            Value::RecFun(rf) => rf.apply_visitor(self),
            Value::NilList => write!(self.buffer, "[]"),
            Value::ConcatList(l) => l.apply_visitor(self),
        }
    }
}

impl<E: Env> Visitor<Function<E>, FmtResult> for PrintVisitor
where
    Self: Visitor<E::Ast, FmtResult> + Visitor<E, FmtResult>,
{
    fn visit(&mut self, node: &Function<E>) -> FmtResult {
        self.parenthesized_visit(&node.env)?;
        self.buffer.write_char('[')?;
        self.buffer.write_str("fun")?;
        self.buffer.write_str(node.bind.as_str())?;
        self.buffer.write_str("->")?;
        node.body.apply_visitor(self)?;
        self.buffer.write_char(']')
    }
}

impl<E: Env> Visitor<RecursiveFunction<E>, FmtResult> for PrintVisitor
where
    Self: Visitor<E::Ast, FmtResult> + Visitor<E, FmtResult>,
{
    fn visit(&mut self, node: &RecursiveFunction<E>) -> FmtResult {
        self.parenthesized_visit(&node.env)?;
        self.buffer.write_char('[')?;
        self.buffer.write_str("rec")?;
        self.buffer.write_str(node.ident.as_str())?;
        self.buffer.write_char('=')?;
        self.buffer.write_str("fun")?;
        self.buffer.write_str(node.bind.as_str())?;
        self.buffer.write_str("->")?;
        node.body.apply_visitor(self)?;
        self.buffer.write_char(']')
    }
}

impl<E: Env> Visitor<ConcatList<E>, FmtResult> for PrintVisitor
where
    PrintVisitor: Visitor<Value<E>, FmtResult>,
{
    fn visit(&mut self, node: &ConcatList<E>) -> FmtResult {
        if matches!(
            node.lhs.as_ref(),
            Value::ConcatList(_) | Value::Fun(_) | Value::RecFun(_)
        ) {
            self.parenthesized_visit(node.lhs.as_ref())?;
        } else {
            node.lhs.apply_visitor(self)?;
        }
        self.buffer.write_str("::")?;
        node.rhs.apply_visitor(self)
    }
}

impl<J: JudgementTrait> Visitor<DerivationTree<J>, FmtResult> for PrintVisitor
where
    Self: Visitor<J, FmtResult>,
{
    fn visit(&mut self, node: &DerivationTree<J>) -> FmtResult {
        node.judgement.apply_visitor(self)?;
        self.buffer.write_str("by")?;
        self.buffer.write_str(node.reason)?;
        if node.premises.is_empty() {
            self.buffer.write_str("{}")
        } else {
            let mut sub_visitor = self.sub_visitor();
            let last_index = node.premises.len() - 1;
            for (index, premise) in node.premises.iter().enumerate() {
                premise.apply_visitor(&mut sub_visitor)?;
                if index != last_index {
                    sub_visitor.buffer.commit_line(true)?;
                }
            }
            self.buffer.commit_block(sub_visitor.buffer.freeze()?)
        }
    }
}

impl<Ast: AstRoot> Visitor<NamedEnv<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &NamedEnv<Ast>) -> FmtResult {
        self.visit_by_iter(
            node.collect(),
            |(id, value), v| {
                v.buffer.write_str(id)?;
                v.buffer.write_char('=')?;
                value.apply_visitor(v)
            },
            |v| v.buffer.write_char(','),
        )
    }
}

impl<Ast: AstRoot> Visitor<ListPatternMatchNode<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &ListPatternMatchNode<Ast>) -> FmtResult {
        self.buffer.write_str("match")?;
        node.expr.apply_visitor(self)?;
        self.buffer.write_str("with")?;
        self.buffer.write_str("[]")?;
        self.buffer.write_str("->")?;
        node.nil_branch.apply_visitor(self)?;
        self.buffer.write_str("|")?;
        self.buffer.write_str(node.head_id.as_str())?;
        self.buffer.write_str("::")?;
        self.buffer.write_str(node.tail_id.as_str())?;
        self.buffer.write_str("->")?;
        node.list_branch.apply_visitor(self)
    }
}

impl Visitor<MonoType, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &MonoType) -> FmtResult {
        match node {
            MonoType::Integer => self.buffer.write_str("int"),
            MonoType::Bool => self.buffer.write_str("bool"),
            MonoType::Var(i) => {
                let entry = self.ty_var_map.entry(*i);

                let ident = entry.or_insert_with(|| self.ident_iter.next().unwrap());
                let result = format!("'{}", ident);
                self.buffer.write_str(result.as_str())
            }
            MonoType::Lambda(box p, box b) => {
                if matches!(p, MonoType::Lambda(_, _) | MonoType::List(_)) {
                    self.parenthesized_visit(p)?;
                } else {
                    p.apply_visitor(self)?;
                }
                self.buffer.write_str("->")?;
                if matches!(b, MonoType::Lambda(_, _) | MonoType::List(_)) {
                    self.parenthesized_visit(b)
                } else {
                    b.apply_visitor(self)
                }
            }
            MonoType::List(box l) => {
                if matches!(l, MonoType::Lambda(_, _) | MonoType::List(_)) {
                    self.parenthesized_visit(l)?;
                } else {
                    l.apply_visitor(self)?;
                }
                self.buffer.write_str("list")
            }
        }
    }
}

impl Visitor<PolyType, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &PolyType) -> FmtResult {
        for bind in node.binds.iter() {
            let entry = self.ty_var_map.entry(*bind);
            let ident = entry.or_insert_with(|| self.ident_iter.next().unwrap());
            let forall = format!("'{}", ident);
            self.buffer.write_str(forall.as_str())?;
        }
        if !node.binds.is_empty() {
            self.buffer.write_char('.')?;
        }
        node.ty.apply_visitor(self)
    }
}

impl<Ast: AstRoot> Visitor<TypedEnv<Ast>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult>,
{
    fn visit(&mut self, node: &TypedEnv<Ast>) -> FmtResult {
        self.visit_by_iter(
            node.env.iter(),
            |(id, ty), v| {
                v.buffer.write_str(id)?;
                v.buffer.write_char(':')?;
                ty.apply_visitor(v)
            },
            |v| v.buffer.write_char(','),
        )
    }
}

impl<Ast: AstRoot> Visitor<TypeJudgement<TypedEnv<Ast>>, FmtResult> for PrintVisitor
where
    Self: Visitor<Ast, FmtResult> + Visitor<TypedEnv<Ast>, FmtResult>,
{
    fn visit(&mut self, node: &TypeJudgement<TypedEnv<Ast>>) -> FmtResult {
        node.env.apply_visitor(self)?;
        self.buffer.write_str("|-")?;
        node.term.apply_visitor(self)?;
        self.buffer.write_str(":")?;
        node.ty.apply_visitor(self)
    }
}
