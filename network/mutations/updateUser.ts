const updateUser = `mutation updateUser($where: UserWhereUniqueInput!, $data: UserUpdateInput!) {
  updateUser(where: $where, data: $data) {
    id
    username
    email
  }
}
`;

export default updateUser;
