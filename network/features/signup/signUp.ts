import { signUp as signUpMutation } from '../../mutations';
import { api } from '../../index';

interface SignUpFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (username: string, email: string, password: string, passwordConfirmation: string): any;
}

const signUp: SignUpFunction = async (username, email, password, passwordConfirmation) => {
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
