import { api } from '../../index';

interface LogOutFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const logOut: LogOutFunction = async () => {
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
