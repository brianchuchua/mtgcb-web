import { authenticateUserWithPassword } from '../../mutations';
import { api } from '../../index';

const logIn = async (username: string, password: string) => {
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
