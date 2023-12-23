const determineSetLabel = (candidateSet: Set): string => {
  let setLabel = 'Set';

  if (candidateSet?.isSubsetGroup) {
    setLabel = 'Subset Group';
  } else if (candidateSet?.parentSetId != null || candidateSet?.subsetGroupId != null) {
    setLabel = 'Subset';
  }

  return setLabel;
};

export default determineSetLabel;

interface Set {
  isSubsetGroup: boolean;
  parentSetId: {
    id: string;
    name: string;
  };
  subsetGroupId?: string;
}
