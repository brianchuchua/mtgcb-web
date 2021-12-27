const setSummaryLegacy = `query setSummaryLegacy($userId: Int!, $setId: Int!){
  setSummaryLegacy(userId: $userId, setId: $setId) {
    setId
    userId
    username
    cardsInSet
    totalCardsCollectedInSet
    uniquePrintingsCollectedInSet
    percentageCollected
    totalValue {
      market
      low
      average
      high
    }
    collection {
        cardID
        quantityReg
        quantityFoil
    }
  }
}`;

export default setSummaryLegacy;
