import { updateUser as updateUserMutation } from '../../mutations';
import { api } from '../../index';

interface UpdatePasswordFunction {
  (id: number, password: string): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const updatePassword: UpdatePasswordFunction = async (id, password) => {
  try {
    const response = await api.post('', {
      query: updateUserMutation,
      variables: {
        id,
        data: {
          password,
        },
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default updatePassword;
