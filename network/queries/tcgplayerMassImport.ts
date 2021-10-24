const tcgplayerMassImport = `query tcgplayerMassImport($setId: Int!, $allCount: Int, $mythicCount: Int, $rareCount: Int, $uncommonCount: Int, $commonCount: Int) {
  tcgplayerMassImport(setId: $setId, allCount: $allCount, mythicCount: $mythicCount, rareCount: $rareCount, uncommonCount: $uncommonCount, commonCount: $commonCount) {
    setId
    tcgplayerMassImport
  }
}
`;

export default tcgplayerMassImport;
