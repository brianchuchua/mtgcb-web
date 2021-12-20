const collectionSummaryLegacy = `query collectionSummaryLegacy($userId: Int!) {
  collectionSummaryLegacy(userId: $userId) {
    userId
    username
    numberOfCardsInMagic
    totalCardsCollected
    uniquePrintingsCollected
    collectionSummary { 
      setId
      cardsInSet
      totalCardsCollectedInSet
      uniquePrintingsCollectedInSet
      market {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        fourOfEachCard
        fourOfEachMythic
        fourOfEachRare
        fourOfEachUncommon
        fourOfEachCommon
        draftCube
      }
      low {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        fourOfEachCard
        fourOfEachMythic
        fourOfEachRare
        fourOfEachUncommon
        fourOfEachCommon
        draftCube
      }
      average {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        fourOfEachCard
        fourOfEachMythic
        fourOfEachRare
        fourOfEachUncommon
        fourOfEachCommon
        draftCube
      }
      high {
        oneOfEachCard
        oneOfEachMythic
        oneOfEachRare
        oneOfEachUncommon
        oneOfEachCommon
        fourOfEachCard
        fourOfEachMythic
        fourOfEachRare
        fourOfEachUncommon
        fourOfEachCommon
        draftCube
      }
    }
  }
}
`;

export default collectionSummaryLegacy;
