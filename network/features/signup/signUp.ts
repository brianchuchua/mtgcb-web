import { signUp as signUpMutation } from '../../mutations';
import { api } from '../../index';

const signUp = async (username: string, email: string, password: string, passwordConfirmation: string) => {
  try {
    const response = await api.post('', {
      query: signUpMutation,
      variables: {
        username,
        email,
        password,
        passwordConfirmation,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default signUp;
