import { api } from '../../index';

const getUser = async () => {
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
