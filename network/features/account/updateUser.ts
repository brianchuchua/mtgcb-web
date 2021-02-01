import { updateUser as updateUserMutation } from '../../mutations';
import { api } from '../../index';

const updateUser = async (id: number, username: string, email: string) => {
  try {
    const response = await api.post('', {
      query: updateUserMutation,
      variables: {
        id,
        data: {
          username,
          email,
        },
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default updateUser;
