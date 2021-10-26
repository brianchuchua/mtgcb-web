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
        draftCube
      }
      low {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        draftCube
      }
      average {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        draftCube
      }
      high {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        draftCube
      }
    }
  }
}
`;

export default costToPurchaseAll;
