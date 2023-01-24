const tcgplayerMassImportForUserLegacy = `query tcgplayerMassImportForUserLegacy($setId: Int!, $userId: Int!, $allCount: Int, $mythicCount: Int, $rareCount: Int, $uncommonCount: Int, $commonCount: Int, $draftCubeCount: Int, $includeSubsetsInSets: Boolean) {
  tcgplayerMassImportForUserLegacy(setId: $setId, userId: $userId, allCount: $allCount, mythicCount: $mythicCount, rareCount: $rareCount, uncommonCount: $uncommonCount, commonCount: $commonCount, draftCubeCount: $draftCubeCount, includeSubsetsInSets: $includeSubsetsInSets) {
    setId
    tcgplayerMassImport
  }
}
`;

export default tcgplayerMassImportForUserLegacy;
