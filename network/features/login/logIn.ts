import { authenticateUserWithPassword } from '../../mutations';
import { api } from '../../index';

interface LogInFunction {
  (username: string, password: string): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const logIn: LogInFunction = async (username, password) => {
  try {
    const response = await api.post('', {
      query: authenticateUserWithPassword,
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

export default logIn;
