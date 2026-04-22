use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub struct VarName {
    pub name: String,
    pub id: usize,
}

// https://stackoverflow.com/questions/41536479/how-do-i-split-an-integer-into-individual-digits
fn digits(mut num: u32) -> impl Iterator<Item = u32> {
    let mut divisor = 1;
    while num >= divisor * 10 {
        divisor *= 10;
    }

    std::iter::from_fn(move || {
        let v = num.checked_div(divisor)?;
        num %= divisor;
        divisor /= 10;
        Some(v)
    })
}

impl fmt::Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.id != 0 {
            for i in digits(self.id as u32) {
                write!(
                    f,
                    "{}",
                    char::from_u32(i + 0x2080).expect("Expected {i} to be a value from 0 to 9")
                )?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Var(VarName),
    Abs(VarName, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

macro_rules! var {
    ($name:expr) => {
        Expr::Var(VarName {
            name: $name.to_string(),
            id: 0,
        })
    };
}

pub(crate) use var;

macro_rules! abs {
    ($name:expr, $body:expr $(,)?) => {
        Expr::Abs(
            VarName {
                name: $name.to_string(),
                id: 0,
            },
            $body.into(),
        )
    };
}

pub(crate) use abs;

macro_rules! abs_bound {
    ($var:expr, $body:expr $(,)?) => {
        Expr::Abs($var, $body.into())
    };
}

macro_rules! app {
    ($lhs:expr, $rhs:expr $(,)?) => {
        Expr::App($lhs.into(), $rhs.into())
    };
}

pub(crate) use app;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(x) => write!(f, "{x}"),
            Expr::Abs(var, body) => write!(f, "λ{var}.{body}"),
            Expr::App(expr1, expr2) => {
                match **expr1 {
                    Expr::Var(_) => write!(f, "{expr1} ")?,
                    _ => write!(f, "({expr1}) ")?,
                };

                match **expr2 {
                    Expr::Var(_) => write!(f, "{expr2}"),
                    _ => write!(f, "({expr2})"),
                }
            }
        }
    }
}

impl Expr {
    pub fn replace(self, var: &VarName, replacement: &Self, var_id: &mut usize) -> Self {
        match self {
            Expr::Var(x) => {
                if x == *var {
                    replacement.clone()
                } else {
                    Expr::Var(x)
                }
            }
            Expr::Abs(x, body) => {
                if x == *var {
                    abs_bound!(x, *body)
                } else if replacement.free_in(&x) {
                    // Alpha-conversion
                    *var_id += 1;
                    let mut fresh = x.clone();
                    fresh.id = *var_id;
                    let renamed_body = body.replace(&x, &Expr::Var(fresh.clone()), var_id);
                    abs_bound!(fresh, renamed_body.replace(var, replacement, var_id))
                } else {
                    abs_bound!(x, body.replace(var, replacement, var_id))
                }
            }
            Expr::App(lhs, rhs) => {
                app!(lhs.replace(var, replacement, var_id), rhs.replace(var, replacement, var_id))
            }
        }
    }

    fn free_in(&self, var: &VarName) -> bool {
        match self {
            Expr::Var(x) => x == var,
            Expr::Abs(x, body) => x != var && body.free_in(var),
            Expr::App(lhs, rhs) => lhs.free_in(var) || rhs.free_in(var),
        }
    }

    pub fn free_variables(self) -> Self {
        match self {
            Expr::Var(VarName { name, .. }) => var!(&name),
            Expr::Abs(var, body) => abs!(&var.name, body.free_variables()),
            Expr::App(lhs, rhs) => app!(lhs.free_variables(), rhs.free_variables()),
        }
    }

    pub fn reduce(self, var_id: &mut usize) -> Self {
        match self {
            Expr::Var(_) => self,
            Expr::Abs(var, body) => {
                let new_body = body.clone().reduce(var_id);
                let ret_body = if new_body != *body { new_body } else { *body };
                abs_bound!(var, ret_body)
            }
            Expr::App(lhs, rhs) => match *lhs {
                Expr::Abs(var, body) => body.replace(&var, &rhs, var_id),
                _ => {
                    let new_lhs = lhs.clone().reduce(var_id);
                    if new_lhs != *lhs {
                        return app!(new_lhs, *rhs);
                    }
                    let new_rhs = rhs.clone().reduce(var_id);
                    if new_rhs != *rhs {
                        return app!(new_lhs, new_rhs);
                    }
                    app!(*lhs, *rhs)
                }
            },
        }
    }

    fn bind_var(self, var: &VarName) -> Self {
        match self {
            Expr::Var(x) if x.name == var.name => Expr::Var(var.clone()),
            Expr::Abs(x, body) => abs_bound!(x, body.bind_var(var)),
            Expr::App(lhs, rhs) => app!(lhs.bind_var(var), rhs.bind_var(var)),
            _ => self,
        }
    }

    pub fn bind_vars(self, global_id: &mut usize) -> Self {
        match self {
            Expr::Var(_) => self,
            Expr::Abs(var, body) => {
                *global_id += 1;
                let var = VarName {
                    name: var.name,
                    id: *global_id,
                };
                abs_bound!(var.clone(), body.bind_var(&var).bind_vars(global_id))
            }
            Expr::App(lhs, rhs) => app!(lhs.bind_vars(global_id), rhs.bind_vars(global_id)),
        }
    }
}
