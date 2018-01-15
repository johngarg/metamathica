(* Lisp-style quotation to leave expressions unevaluated. Implementation taken from this answer:
   https://mathematica.stackexchange.com/questions/103263/generating-assignments-and-transforming-code-inside-held-forms-when-generating-c/103286
 *)

ClearAll[quote, quasiquote, unquote, quoteEval, quoted];

SetAttributes[{quote, quasiquote, unquote, quoted}, HoldAllComplete];

Options[quote] = {
  "ExcludedForms" -> List | Rule | RuleDelayed | Hold | HoldComplete |
                     HoldForm | Slot | SlotSequence | Alternatives | Default |
                     Blank | BlankSequence | BlankNullSequence | Except |
                     Optional | Pattern | PatternTest | PatternSequence |
                     Repeated | RepeatedNull | Longest | Shortest | Verbatim |
                     HoldPattern | OptionsPattern | IgnoringInactive | _quoted |
                     _CompiledFunction | _Experimental`NumericalFunction |
                     _InterpolatingFunction | _BSplineFunction |
                     _BezierFunction | _ParametricFunction | _NDSolve`StateData |
                     _NDSolve`FiniteDifferenceDerivative | _BooleanFunction |
                     _Root | _AlgebraicNumber | _DifferenceRoot |
                     _DifferentialRoot | _FittedModel | _DataDistribution |
                     _SurvivalModel | _NearestFunction | _LinearSolveFunction |
                     _Image | _Image3D | _StateSpaceModel |
                     _TransferFunctionModel | _Graphics | _Graphics3D |
                     _GraphicsComplex | _Point | _Line | _Polygon | _Directive |
                     _TemporalData | _CellObject | _ColorDataFunction |
                     _ColorProfileData | _ByteArray | _CoxModel | _EventData |
                     _HypothesisTestData | _LibraryFunction |
                     _LiftingFilterData | _NotebookObject | _OptimumFlowData |
                     _SeriesData | _StructuredArray | _WeightedData,
  "ExtraRules" -> {}};

quote[expr_, extraExclusion : Except[_?OptionQ] | PatternSequence[],
      opts___?OptionQ] :=
  Unevaluated[expr] /. Flatten@{
    OptionValue[quote, {opts}, "ExtraRules"],
    excluded : extraExclusion | OptionValue[quote, {opts}, "ExcludedForms"] :> excluded,
    sym : Except[HoldPattern@Symbol[___], _Symbol] :> quoted[sym]};

quasiquote[
  expr_, extraExclusion : Except[_?OptionQ] | PatternSequence[], opts___?OptionQ] :=
  quote[expr, extraExclusion | _unquote, opts];

unquote[expr_] := expr;

Options[quoteEval] = {"Head" -> Identity, "Pattern" -> _};
quoteEval[expr_, opts___?OptionQ] :=
  OptionValue[quoteEval, {opts}, "Head"][expr] /. HoldPattern[quoted][
    x : OptionValue[quoteEval, {opts}, "Pattern"]] :> x;
