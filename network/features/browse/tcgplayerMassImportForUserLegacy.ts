import { api } from '../../index';
import { tcgplayerMassImportForUserLegacy as tcgplayerMassImportForUserLegacyQuery } from '../../queries';
import { TcgplayerMassImportForUserLegacyOptions } from './commonTypes';

interface TcgplayerMassImportForUserLegacyFunction {
  (tcgplayerOptions: TcgplayerMassImportForUserLegacyOptions): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

// TODO: Migrate me to redux query
const tcgplayerMassImportForUserLegacy: TcgplayerMassImportForUserLegacyFunction = async (tcgplayerOptions) => {
  const {
    setId,
    userId,
    allCount,
    mythicCount,
    rareCount,
    uncommonCount,
    commonCount,
    draftCubeCount,
    includeSubsetsInSets,
  } = tcgplayerOptions;

  try {
    const response = await api.post('', {
      query: tcgplayerMassImportForUserLegacyQuery,
      variables: {
        setId,
        userId: Number(userId),
        allCount,
        mythicCount,
        rareCount,
        uncommonCount,
        commonCount,
        draftCubeCount,
        includeSubsetsInSets,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default tcgplayerMassImportForUserLegacy;
