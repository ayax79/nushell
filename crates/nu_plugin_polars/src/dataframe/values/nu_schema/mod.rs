pub mod custom_value;

use std::sync::Arc;

use custom_value::NuSchemaCustomValue;
use nu_protocol::{ShellError, Span, Value};
use polars::prelude::{Schema, SchemaExt, SchemaRef};
use uuid::Uuid;

use crate::{Cacheable, PolarsPlugin};

use super::{
    nu_dtype::{fields_to_value, value_to_fields},
    CustomValueSupport, PolarsPluginObject, PolarsPluginType,
};

#[derive(Debug, Clone)]
pub struct NuSchema {
    pub id: Uuid,
    pub schema: SchemaRef,
}

impl NuSchema {
    pub fn new(schema: SchemaRef) -> Self {
        Self {
            id: Uuid::new_v4(),
            schema,
        }
    }
}

impl From<NuSchema> for SchemaRef {
    fn from(val: NuSchema) -> Self {
        Arc::clone(&val.schema)
    }
}

impl From<SchemaRef> for NuSchema {
    fn from(val: SchemaRef) -> Self {
        Self::new(val)
    }
}

impl Cacheable for NuSchema {
    fn cache_id(&self) -> &Uuid {
        &self.id
    }

    fn to_cache_value(&self) -> Result<super::PolarsPluginObject, ShellError> {
        Ok(PolarsPluginObject::NuSchema(self.clone()))
    }

    fn from_cache_value(cv: super::PolarsPluginObject) -> Result<Self, ShellError> {
        match cv {
            PolarsPluginObject::NuSchema(dt) => Ok(dt),
            _ => Err(ShellError::GenericError {
                error: "Cache value is not a dataframe".into(),
                msg: "".into(),
                span: None,
                help: None,
                inner: vec![],
            }),
        }
    }
}

impl CustomValueSupport for NuSchema {
    type CV = NuSchemaCustomValue;

    fn get_type_static() -> super::PolarsPluginType {
        PolarsPluginType::NuSchema
    }

    fn custom_value(self) -> Self::CV {
        NuSchemaCustomValue {
            id: self.id,
            datatype: Some(self),
        }
    }

    fn base_value(self, span: Span) -> Result<Value, ShellError> {
        Ok(fields_to_value(self.schema.iter_fields(), span))
    }

    fn try_from_value(plugin: &PolarsPlugin, value: &Value) -> Result<Self, ShellError> {
        if let Value::Custom { val, .. } = value {
            if let Some(cv) = val.as_any().downcast_ref::<Self::CV>() {
                Self::try_from_custom_value(plugin, cv)
            } else {
                Err(ShellError::CantConvert {
                    to_type: Self::get_type_static().to_string(),
                    from_type: value.get_type().to_string(),
                    span: value.span(),
                    help: None,
                })
            }
        } else {
            let schema = value_to_schema(plugin, value, Span::unknown())?;
            Ok(Self::new(Arc::new(schema)))
        }
    }
}

fn value_to_schema(plugin: &PolarsPlugin, value: &Value, span: Span) -> Result<Schema, ShellError> {
    let fields = value_to_fields(plugin, value, span)?;
    let schema = Schema::from_iter(fields);
    Ok(schema)
}
