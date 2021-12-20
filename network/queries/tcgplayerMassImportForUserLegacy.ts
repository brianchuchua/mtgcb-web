const tcgplayerMassImportForUserLegacy = `query tcgplayerMassImportForUserLegacy($setId: Int!, $userId: Int!, $allCount: Int, $mythicCount: Int, $rareCount: Int, $uncommonCount: Int, $commonCount: Int, $draftCubeCount: Int) {
  tcgplayerMassImportForUserLegacy(setId: $setId, userId: $userId, allCount: $allCount, mythicCount: $mythicCount, rareCount: $rareCount, uncommonCount: $uncommonCount, commonCount: $commonCount, draftCubeCount: $draftCubeCount) {
    setId
    tcgplayerMassImport
  }
}
`;

export default tcgplayerMassImportForUserLegacy;
