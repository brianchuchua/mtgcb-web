import { updateUser as updateUserMutation } from '../../mutations';
import { api } from '../../index';

const updatePassword = async (id: number, password: string) => {
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
