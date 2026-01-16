use crate::{
   Tok,
   pipelines::parser::{
      ParseBuffer,
      syntax::parse::{Parse, ParseError, Result},
   },
};
use diag::DiagSink;

macro_rules! define_sync_point {
   ($type:ident {
      $($sync_point:ty),* $(,)?
   }) => {
      #[derive(Debug, Clone)]
      pub struct $type;

      impl Parse for $type {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            $(
               if input.try_parse::<$sync_point>(sink)?.is_some() {
                  return Ok(Self);
               };
            )*

            Err(ParseError::Never)
         }
      }
   };
}

define_sync_point! (SyncPoint {
   Tok![;],
   Tok![,],
});

define_sync_point! (TypeSyncPoint {
   Tok![;],
   Tok![,],
   Tok![=],
});
