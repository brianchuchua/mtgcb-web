import { useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { useGetAllSetNamesQuery } from '../../../../network/services/mtgcbApi';
import { CardSet, setCardSets } from '../../browseSlice';
import { mapCardSets } from '../mappers';

const SetSelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateSets = (newSets: CardSet[]) => {
    dispatch(setCardSets({ cardSets: newSets }));
  };

  const [selectedSets, setSelectedSets] = useState([]);

  const { data: allSetsMetaResponse } = useGetAllSetNamesQuery({});
  const allSetNames = allSetsMetaResponse?.data?.allSets;
  const sets = mapCardSets(allSetNames || []);

  return (
    sets && (
      <StyledSetSelector>
        <AutocompleteWithNegation
          label="Card Sets"
          options={sets}
          selectedOptions={selectedSets}
          setSelectedOptionsLocally={setSelectedSets}
          setSelectedOptionsRemotely={updateSets}
        />
      </StyledSetSelector>
    )
  );
};

const StyledSetSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default SetSelector;
