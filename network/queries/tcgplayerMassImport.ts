const tcgplayerMassImport = `query tcgplayerMassImport($setId: Int!, $allCount: Int, $mythicCount: Int, $rareCount: Int, $uncommonCount: Int, $commonCount: Int, $draftCubeCount: Int) {
  tcgplayerMassImport(setId: $setId, allCount: $allCount, mythicCount: $mythicCount, rareCount: $rareCount, uncommonCount: $uncommonCount, commonCount: $commonCount, draftCubeCount: $draftCubeCount) {
    setId
    tcgplayerMassImport
  }
}
`;

export default tcgplayerMassImport;
