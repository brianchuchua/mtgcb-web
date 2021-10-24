import { useEffect, useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { getCardSets } from '../../../../network/features/browse';
import { CardSet, setCardSets } from '../../browseSlice';
import { mapCardSets } from '../mappers';

const SetSelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateSets = (newSets: CardSet[]) => {
    dispatch(setCardSets({ cardSets: newSets }));
  };

  const [sets, setSets] = useState(null);
  const [selectedSets, setSelectedSets] = useState([]);

  useEffect(() => {
    async function fetchSets() {
      const setsResponse = await getCardSets({ first: 1000 }); // TODO: This is being called twice in the app, lift it up in state or make the first call lighter
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
