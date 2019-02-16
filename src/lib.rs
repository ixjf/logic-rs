extern crate console_error_panic_hook;
extern crate wasm_bindgen;
#[macro_use]
extern crate serde_derive;
extern crate logic_rs;

use wasm_bindgen::prelude::*;

#[derive(Serialize)]
struct IsConsistent {
    pub is_consistent: bool,
    pub truth_tree: logic_rs::TruthTree,
}

#[derive(Serialize)]
struct IsValid {
    pub is_valid: bool,
    pub truth_tree: logic_rs::TruthTree,
}

#[derive(Serialize)]
struct IsContradiction {
    pub is_contradiction: bool,
    pub truth_tree: logic_rs::TruthTree,
}

#[derive(Serialize)]
struct IsTautology {
    pub is_tautology: bool,
    pub truth_tree: logic_rs::TruthTree,
}

#[derive(Serialize)]
struct IsContingency {
    pub is_contingency: bool,
    pub contradiction_truth_tree: logic_rs::TruthTree,
    pub tautology_truth_tree: logic_rs::TruthTree,
}

#[wasm_bindgen]
pub enum InputKind {
    StatementSet,
    Argument,
    Statement,
}

#[derive(Serialize)]
pub enum ValidityError {
    NotAStatementSet,
    NotAnArgument,
    NotASingleStatement,
}

#[derive(Serialize)]
struct ParserError {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

#[wasm_bindgen]
pub struct ParsedInput {
    kind: logic_rs::InputKind,
}

#[wasm_bindgen]
impl ParsedInput {
    pub fn get_kind(&self) -> InputKind {
        match self.kind {
            logic_rs::InputKind::StatementSet(_) => InputKind::StatementSet,
            logic_rs::InputKind::Argument(_) => InputKind::Argument,
            logic_rs::InputKind::Statement(_) => InputKind::Statement,
        }
    }

    pub fn is_consistent(&self) -> Result<JsValue, JsValue> {
        match self.kind {
            logic_rs::InputKind::StatementSet(ref st_set) => {
                let (is_consistent, truth_tree) = st_set.is_consistent();

                Ok(JsValue::from_serde(&IsConsistent {
                    is_consistent,
                    truth_tree,
                })
                .unwrap())
            }
            _ => Err(JsValue::from_serde(&ValidityError::NotAStatementSet).unwrap()),
        }
    }

    pub fn is_valid(&self) -> Result<JsValue, JsValue> {
        match self.kind {
            logic_rs::InputKind::Argument(ref argument) => {
                let (is_valid, truth_tree) = argument.is_valid();

                Ok(JsValue::from_serde(&IsValid {
                    is_valid,
                    truth_tree,
                })
                .unwrap())
            }
            _ => Err(JsValue::from_serde(&ValidityError::NotAnArgument).unwrap()),
        }
    }

    pub fn is_contradiction(&self) -> Result<JsValue, JsValue> {
        match self.kind {
            logic_rs::InputKind::Statement(ref single_statement) => {
                let (is_contradiction, truth_tree) = single_statement.is_contradiction();

                Ok(JsValue::from_serde(&IsContradiction {
                    is_contradiction,
                    truth_tree,
                })
                .unwrap())
            }
            _ => Err(JsValue::from_serde(&ValidityError::NotASingleStatement).unwrap()),
        }
    }

    pub fn is_tautology(&self) -> Result<JsValue, JsValue> {
        match self.kind {
            logic_rs::InputKind::Statement(ref single_statement) => {
                let (is_tautology, truth_tree) = single_statement.is_tautology();

                Ok(JsValue::from_serde(&IsTautology {
                    is_tautology,
                    truth_tree,
                })
                .unwrap())
            }
            _ => Err(JsValue::from_serde(&ValidityError::NotASingleStatement).unwrap()),
        }
    }

    pub fn is_contingency(&self) -> Result<JsValue, JsValue> {
        match self.kind {
            logic_rs::InputKind::Statement(ref single_statement) => {
                let (is_contingency, contradiction_truth_tree, tautology_truth_tree) =
                    single_statement.is_contingency();

                Ok(JsValue::from_serde(&IsContingency {
                    is_contingency,
                    contradiction_truth_tree,
                    tautology_truth_tree,
                })
                .unwrap())
            }
            _ => Err(JsValue::from_serde(&ValidityError::NotASingleStatement).unwrap()),
        }
    }
}

#[wasm_bindgen]
pub fn parse_input(input: &str) -> Result<ParsedInput, JsValue> {
    // Have the panic hook initialize once in the lifetime of this wasm
    // Gives us pretty errors on the browser console rather than just e.g. "Unreachable executed"
    console_error_panic_hook::set_once();

    match logic_rs::parse_input(input) {
        Ok(input_kind) => Ok(ParsedInput { kind: input_kind }),
        Err(e) => Err(JsValue::from_serde(&ParserError {
            line: e.location.0,
            col: e.location.1,
            message: format!("{}", e),
        })
        .unwrap()),
    }
}
