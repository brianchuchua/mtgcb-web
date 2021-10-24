import { api } from '../../index';
import { tcgplayerMassImport as tcgplayerMassImportQuery } from '../../queries';
import { TcgplayerMassImportOptions } from './commonTypes';

interface TcgplayerMassImportFunction {
  (tcgplayerOptions: TcgplayerMassImportOptions): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const tcgplayerMassImport: TcgplayerMassImportFunction = async (tcgplayerOptions) => {
  const { setId, allCount, mythicCount, rareCount, uncommonCount, commonCount } = tcgplayerOptions;

  try {
    const response = await api.post('', {
      query: tcgplayerMassImportQuery,
      variables: {
        setId,
        allCount,
        mythicCount,
        rareCount,
        uncommonCount,
        commonCount,
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default tcgplayerMassImport;
