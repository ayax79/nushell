use crate::PolarsPlugin;
use crate::dataframe::values::NuExpression;
use crate::values::{CustomValueSupport, PolarsPluginObject, PolarsPluginType, cant_convert_err};
use nu_plugin::{EngineInterface, EvaluatedCall, PluginCommand};
use nu_protocol::engine::Closure;
use nu_protocol::{
    Category, Example, LabeledError, PipelineData, ShellError, Signature, Span, Spanned,
    SyntaxShape, Value,
};
use polars::error::PolarsError;
use polars::prelude::{DataType, Field};

#[derive(Clone)]
pub struct ExprMap;

impl PluginCommand for ExprMap {
    type Plugin = PolarsPlugin;

    fn name(&self) -> &str {
        "polars map"
    }

    fn description(&self) -> &str {
        "Apply a function/closure once the logical plan is executed."
    }

    fn signature(&self) -> Signature {
        Signature::build(self.name())
            // todo: update this to a column type once it has been implemented
            .required(
                "closure",
                SyntaxShape::Closure(Some(vec![SyntaxShape::Any])),
                "The closure to apply to the expression.",
            )
            .input_output_types(vec![(
                PolarsPluginType::NuExpression.into(),
                PolarsPluginType::NuExpression.into(),
            )])
            .category(Category::Custom("expression".into()))
    }

    fn examples(&self) -> Vec<Example<'_>> {
        vec![]
    }

    fn run(
        &self,
        plugin: &Self::Plugin,
        engine: &EngineInterface,
        call: &EvaluatedCall,
        input: PipelineData,
    ) -> Result<PipelineData, LabeledError> {
        let metadata = input.metadata();
        let value = input.into_value(call.head)?;

        match PolarsPluginObject::try_from_value(plugin, &value)? {
            PolarsPluginObject::NuExpression(expr) => command(plugin, engine, call, expr),
            _ => Err(cant_convert_err(&value, &[PolarsPluginType::NuExpression])),
        }
        .map_err(LabeledError::from)
        .map(|pd| pd.set_metadata(metadata))
    }
}

pub(crate) fn command(
    plugin: &PolarsPlugin,
    e: &EngineInterface,
    call: &EvaluatedCall,
    expr: NuExpression,
) -> Result<PipelineData, ShellError> {
    // let dtype: Spanned<NuDataType> = call.req::<Value>(0).and_then(|v| {
    //     NuDataType::try_from_value(plugin, &v).map(|dt| Spanned {
    //         item: dt,
    //         span: v.span(),
    //     })
    // })?;
    let closure: Spanned<Closure> = call.req(1)?;
    let engine = e.clone();

    let expr: NuExpression = expr
        .into_polars()
        .map(
            move |column| {
                engine
                    .eval_closure(
                        &closure,
                        vec![Value::string("fix me", Span::unknown())],
                        None,
                    )
                    .map_err(|e| {
                        PolarsError::ComputeError(
                            format!("Error evaluating closure in map expression: {}", e).into(),
                        )
                    })
                    .map(|_| column)
            },
            |_schema, _field| Ok(Field::new("boop".into(), DataType::Boolean)),
        )
        .into();

    expr.to_pipeline_data(plugin, e, call.head)
}
