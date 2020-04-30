open Jest;

open Expect;

type messageOk = string;
type messageError = string;
type t = DBErrors.t(messageOk, messageError);

describe("DBErrors", () => {
  describe("Success('a)", () => {
    let actual = DBErrors.Success("");
    test("isSuccess to be true", () =>
      actual |> DBErrors.isSuccess |> expect |> toBe(true)
    );
    test("isFailure to be false", () =>
      actual |> DBErrors.isFailure |> expect |> toBe(false)
    );
  });
  describe("Failure('e)", () => {
    let actual = DBErrors.Failure("");

    test("isSuccess to be false", () =>
      actual |> DBErrors.isSuccess |> expect |> toBe(false)
    );
    test("isFailure to be true", () =>
      actual |> DBErrors.isFailure |> expect |> toBe(true)
    );
  });
  describe("andMap", () => {
    test("Success", () =>
      DBErrors.andMap(
        DBErrors.Success("before"),
        DBErrors.Success(before => before ++ " after"),
      )
      |> expect
      |> toEqual(DBErrors.Success("before after"))
    )
  });
  describe("map", () => {
    let mapper = before => before ++ " after";
    test("Success", () =>
      DBErrors.map(mapper, DBErrors.Success("before"))
      |> expect
      |> toEqual(DBErrors.Success("before after"))
    );
    test("Failure", () =>
      DBErrors.map(mapper, DBErrors.Failure("failure"))
      |> expect
      |> toEqual(DBErrors.Failure("failure"))
    );
  });
  describe("map2", () => {
    let mapper = (before1, before2) => before1 ++ " " ++ before2 ++ " after";
    test("Success", () =>
      DBErrors.map2(
        mapper,
        DBErrors.Success("before1"),
        DBErrors.Success("before2"),
      )
      |> expect
      |> toEqual(DBErrors.Success("before1 before2 after"))
    );
    test("Failure - first", () =>
      DBErrors.map2(
        mapper,
        DBErrors.Failure("failure1"),
        DBErrors.Failure("failure2"),
      )
      |> expect
      |> toEqual(DBErrors.Failure("failure1"))
    );
    test("Failure - second", () =>
      DBErrors.map2(
        mapper,
        DBErrors.Success("failure1"),
        DBErrors.Failure("failure2"),
      )
      |> expect
      |> toEqual(DBErrors.Failure("failure2"))
    );
  });
  describe("map3", () => {
    let mapper = (before1, before2, before3) =>
      before1 ++ " " ++ before2 ++ " " ++ before3 ++ " after";
    test("Success", () =>
      DBErrors.map3(
        mapper,
        DBErrors.Success("before1"),
        DBErrors.Success("before2"),
        DBErrors.Success("before3"),
      )
      |> expect
      |> toEqual(DBErrors.Success("before1 before2 before3 after"))
    );
    test("Failure - first", () =>
      DBErrors.map3(
        mapper,
        DBErrors.Failure("failure1"),
        DBErrors.Failure("failure2"),
        DBErrors.Failure("failure3"),
      )
      |> expect
      |> toEqual(DBErrors.Failure("failure1"))
    );
    test("Failure - second", () =>
      DBErrors.map3(
        mapper,
        DBErrors.Success("failure1"),
        DBErrors.Failure("failure2"),
        DBErrors.Failure("failure3"),
      )
      |> expect
      |> toEqual(DBErrors.Failure("failure2"))
    );
    test("Failure - third", () =>
      DBErrors.map3(
        mapper,
        DBErrors.Success("failure1"),
        DBErrors.Success("failure2"),
        DBErrors.Failure("failure3"),
      )
      |> expect
      |> toEqual(DBErrors.Failure("failure3"))
    );
  });
  describe("mapError", () => {
    let mapper = before => before ++ " after";
    test("Success", () =>
      DBErrors.mapError(mapper, DBErrors.Success("before"))
      |> expect
      |> toEqual(DBErrors.Success("before"))
    );
    test("Failure", () =>
      DBErrors.mapError(mapper, DBErrors.Failure("before"))
      |> expect
      |> toEqual(DBErrors.Failure("before after"))
    );
  });
  describe("mapBoth", () => {
    let successMapper = before => before ++ " after success";
    let errorMapper = before => before ++ " after error";
    test("Success", () =>
      DBErrors.mapBoth(
        successMapper,
        errorMapper,
        DBErrors.Success("before"),
      )
      |> expect
      |> toEqual(DBErrors.Success("before after success"))
    );
    test("Failure", () =>
      DBErrors.mapBoth(
        successMapper,
        errorMapper,
        DBErrors.Failure("before"),
      )
      |> expect
      |> toEqual(DBErrors.Failure("before after error"))
    );
  });
  describe("andThen", () => {
    let mapper = before => DBErrors.Failure(before ++ " after");
    test("Success", () =>
      DBErrors.andThen(mapper, DBErrors.Success("before"))
      |> expect
      |> toEqual(DBErrors.Failure("before after"))
    );
    test("Failure", () =>
      DBErrors.andThen(mapper, DBErrors.Failure("before"))
      |> expect
      |> toEqual(DBErrors.Failure("before"))
    );
  });
  describe("withDefault", () => {
    test("Success", () =>
      DBErrors.withDefault("got default", DBErrors.Success("got success"))
      |> expect
      |> toBe("got success")
    );
    test("Failure", () =>
      DBErrors.withDefault("got default", DBErrors.Failure("got failure"))
      |> expect
      |> toBe("got default")
    );
  });
  describe("fromResult", () => {
    test("Ok", () =>
      DBErrors.fromResult(Belt.Result.Ok("from result"))
      |> expect
      |> toEqual(DBErrors.Success("from result"))
    );
    test("Error", () =>
      DBErrors.fromResult(Belt.Result.Error("from result"))
      |> expect
      |> toEqual(DBErrors.Failure("from result"))
    );
  });
  describe("toOption", () => {
    test("Success", () =>
      DBErrors.toOption(DBErrors.Success("got success"))
      |> expect
      |> toEqual(Some("got success"))
    );
    test("Failure", () =>
      DBErrors.toOption(DBErrors.Failure("got failure"))
      |> expect
      |> toEqual(None)
    );
  });
  describe("append", () => {
    test("Success(a) + Success(b) = Success((a, b))", () =>
      DBErrors.append(DBErrors.Success("a"), DBErrors.Success("b"))
      |> expect
      |> toEqual(DBErrors.Success(("a", "b")))
    );
    test("Failure(a) + Success(b) = Failure(a)", () =>
      DBErrors.append(DBErrors.Failure("a"), DBErrors.Success("b"))
      |> expect
      |> toEqual(DBErrors.Failure("a"))
    );
    test("Success(a) + Failure(b)", () =>
      DBErrors.append(DBErrors.Success("a"), DBErrors.Failure("b"))
      |> expect
      |> toEqual(DBErrors.Failure("b"))
    );
  });
  describe("succeed", () => {
    test("a => Success(a)", () =>
      DBErrors.succeed("got success")
      |> expect
      |> toEqual(DBErrors.Success("got success"))
    );
    ignore();
  });
  describe("isSuccess", () => {
    test("Success returns true", () =>
      DBErrors.isSuccess(DBErrors.Success("got success"))
      |> expect
      |> toBe(true)
    );
    test("Failure returns false", () =>
      DBErrors.isSuccess(DBErrors.Failure("got failure"))
      |> expect
      |> toBe(false)
    );
  });
  describe("isFailure", () => {
    test("Success returns false", () =>
      DBErrors.isFailure(DBErrors.Success("got success"))
      |> expect
      |> toBe(false)
    );
    test("Failure returns true", () =>
      DBErrors.isFailure(DBErrors.Failure("got failure"))
      |> expect
      |> toBe(true)
    );
  });
});