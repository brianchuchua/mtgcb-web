const allCardsMeta = `query _allCardsMeta($name: String = "") {
  _allCardsMeta(search: $name) {
    count
  }
}
`;

export default allCardsMeta;
