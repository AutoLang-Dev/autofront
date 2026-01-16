use syntax::Tok;

macro_rules! define_sync_point {
   ($type:ident {
      $($sync_point:ty),* $(,)?
   }) => {
      #[derive(Debug, Clone)]
      pub struct $type;

      impl crate::syntax::parse::Parse for $type {
         fn parse(input: &crate::buffer::ParseBuffer, sink: &mut diag::DiagSink) -> crate::syntax::parse::Result<Self> {
            $(
               if input.try_parse::<$sync_point>(sink)?.is_some() {
                  return Ok(Self);
               };
            )*

            Err(crate::syntax::parse::ParseError::Never)
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
