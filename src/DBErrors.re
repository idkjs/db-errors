type t('string,'error) =
  | Failure('error)
  | Success('string);

/* Flips the order of calling passed in paramater from a => b => c to callin c first then b of c, then then result to a, returns the result of a */
let (@!) = (a, b, c) => a(b(c));

let andMap = (wrappedValue, wrappedFunction) =>
  switch (wrappedFunction, wrappedValue) {
  // | (Success(f), Success(value)) => Success(f @@ value)
  | (Success(f), Success(value)) => Success(f(value))
  | (Failure(error), _) => Failure(error)
  | (_, Failure(error)) => Failure(error)
  };

let map = (f, data) =>
  switch (data) {
  | Success(value) => Success(f(value))
  | Failure(e) => Failure(e)
  };

let map2 = (f, a, b) => map(f, a) |> andMap(b);

let map3 = (f, a, b, c) => map(f, a) |> andMap(b) |> andMap(c);

let mapError = (f, data) =>
  switch (data) {
  | Success(x) => Success(x)
  | Failure(e) => Failure(f @@ e)
  };

let mapBoth = (successFn, errorFn) => mapError(errorFn) @! map(successFn);

let andThen = (f, data) =>
  switch (data) {
  | Success(a) => f(a)
  | Failure(_) => data
  };

let withDefault = (default, data) =>
  switch (data) {
  | Success(x) => x
  | _ => default
  };

let fromResult =
  Belt.Result.(
    fun
    | Ok(x) => Success(x)
    | Error(e) => Failure(e)
  );

let toOption =
  fun
  | Success(a) => Some(a)
  | _ => None;

let append = (a, b) => map((a, b) => (a, b), a) |> andMap(b);

let succeed = a => Success(a);

let isSuccess =
  fun
  | Success(_a) => true
  | _ => false;

let isFailure =
  fun
  | Failure(_e) => true
  | _ => false;



