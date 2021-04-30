import { useEffect, useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import { setCardSets, CardSet } from '../../browseSlice';
import { getCardSets } from '../../../../network/features/browse';
import { mapCardSets } from '../mappers';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';

const SetSelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateSets = (newSets: CardSet[]) => {
    dispatch(setCardSets({ cardSets: newSets }));
  };

  const [sets, setSets] = useState(null);
  const [selectedSets, setSelectedSets] = useState([]);

  useEffect(() => {
    async function fetchSets() {
      const setsResponse = await getCardSets();
      const allSets = setsResponse?.data?.data?.allSets;
      const allSetsMapped = mapCardSets(allSets);

      setSets(allSetsMapped);
    }
    fetchSets();
  }, []);

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
