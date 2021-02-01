import { api } from '../../index';

const logOut = async () => {
  try {
    const response = await api.post('', {
      query: `mutation {
        unauthenticate: unauthenticateUser {
          success
        }
      }      
      `,
    });

    return response;
  } catch (error) {
    return null;
  }
};

export default logOut;
