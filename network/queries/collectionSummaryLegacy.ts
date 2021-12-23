const collectionSummaryLegacy = `query collectionSummaryLegacy($userId: Int!) {
  collectionSummaryLegacy(userId: $userId) {
    userId
    username
    numberOfCardsInMagic
    totalCardsCollected
    uniquePrintingsCollected
    percentageCollected
    totalValue {
      market
      low
      average
      high
    }
    collectionSummary { 
      setId
      cardsInSet
      totalCardsCollectedInSet
      uniquePrintingsCollectedInSet
      percentageCollected
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
        totalValue
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
        totalValue
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
        totalValue
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
        totalValue
      }
    }
  }
}
`;

export default collectionSummaryLegacy;
