module Tests

// https://github.com/Zaid-Ajaj/Fable.Mocha
open Fable.Mocha

let appTests =
    testList "app" [
        // let update (msg: Message) (model: Model) : Model * Cmd<Message> =
        testList "update" [
            testCase "update happy"
            <| fun () ->
                let initModel = App.init () |> fst
                let msg = App.Message.AuthFinished(Error(System.Exception "I'm still happy anyway"))
                let actual = App.update msg initModel |> fst
                Expect.isSome actual.AuthInfo "AuthInfo"

        ]

    ]

let arithmeticTests =
    testList "Arithmetic tests" [
        test "plus works" { Expect.equal (1 + 1) 2 "plus" }

        test "Test for falsehood" { Expect.isFalse (1 = 2) "false" }

        testAsync "Test async code" {
            let! x = async { return 21 }
            let answer = x * 2
            Expect.equal 42 answer "async"
        }
    ]

Mocha.runTests arithmeticTests |> ignore
Mocha.runTests appTests |> ignore
