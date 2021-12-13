import { api } from '../../index';
import { authenticateUserWithPasswordLegacy } from '../../mutations';

interface MigrateLegacyAccountFunction {
  (username: string, password: string): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const migrateLegacyAccount: MigrateLegacyAccountFunction = async (username, password) => {
  try {
    const response = await api.post('', {
      query: authenticateUserWithPasswordLegacy,
      variables: {
        username,
        password,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default migrateLegacyAccount;
