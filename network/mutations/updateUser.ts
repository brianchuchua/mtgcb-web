const updateUser = `mutation updateUser($id: ID!, $data: UserUpdateInput!) {
  updateUser(id: $id, data: $data) {
    id
    username
    email
  }
}
`;

export default updateUser;
