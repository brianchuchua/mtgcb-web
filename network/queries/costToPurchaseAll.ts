const costToPurchaseAll = `query costToPurchaseAll {
  costToPurchaseAll {
    costToPurchaseAll { 
      setId
      market {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
      }
      low {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
      }
      average {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
      }
      high {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
      }
    }
  }
}
`;

export default costToPurchaseAll;
