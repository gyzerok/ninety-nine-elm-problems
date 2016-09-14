module Browser exposing (..)

import Test.Runner.Html
import Tests


main : Program Never
main =
    Test.Runner.Html.run Tests.all
