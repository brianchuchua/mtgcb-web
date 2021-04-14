import { api } from '../../index';

interface GetUserFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getUser: GetUserFunction = async () => {
  try {
    const response = await api.post('', {
      query: `query authenticatedUser {
        authenticatedUser {
          id
          username
          email
        }
      }
      `,
    });

    return response;
  } catch (error) {
    return null;
  }
};

export default getUser;
