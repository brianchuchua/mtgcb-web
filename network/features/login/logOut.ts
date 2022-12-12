import { api } from '../../index';

interface LogOutFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const logOut: LogOutFunction = async () => {
  try {
    const response = await api.post('', {
      query: `mutation {
        endSession
      }      
      `,
    });

    return response;
  } catch (error) {
    return null;
  }
};

export default logOut;
