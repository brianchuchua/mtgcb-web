import { api } from '../../index';
import { updateUser as updateUserMutation } from '../../mutations';

interface UpdateUserFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (id: number, username: string, email: string): any;
}

const updateUser: UpdateUserFunction = async (id, username, email) => {
  try {
    const response = await api.post('', {
      query: updateUserMutation,
      variables: {
        where: { id },
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
