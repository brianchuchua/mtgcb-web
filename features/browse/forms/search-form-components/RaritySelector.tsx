import { useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { CardRarity, rarityOptions, setCardRarities } from '../../browseSlice';

const RaritySelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateRarities = (selectedRarities: CardRarity[]) => {
    dispatch(setCardRarities({ cardRarities: selectedRarities }));
  };

  const [selectedRarities, setSelectedRarities] = useState([]);

  return (
    <StyledRaritySelector>
      <AutocompleteWithNegation
        label="Rarities"
        options={rarityOptions}
        selectedOptions={selectedRarities}
        setSelectedOptionsLocally={setSelectedRarities}
        setSelectedOptionsRemotely={updateRarities}
      />
    </StyledRaritySelector>
  );
};

const StyledRaritySelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default RaritySelector;
