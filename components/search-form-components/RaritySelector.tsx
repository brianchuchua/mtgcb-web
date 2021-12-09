import styled from 'styled-components';
import { CardRarity } from '../../features/browse/browseSlice';
import AutocompleteWithNegation from '../AutocompleteWithNegation';

interface RarityOption {
  category: string;
  value: string;
  label: string;
  exclude: boolean;
}

interface RaritySelectorProps {
  rarityOptions: RarityOption[];
  cardRarities: CardRarity[];
  updateRarities: (selectedRarities: CardRarity[]) => void;
}

const RaritySelector: React.FC<RaritySelectorProps> = ({ rarityOptions, cardRarities, updateRarities }) => (
  <StyledRaritySelector>
    <AutocompleteWithNegation
      label="Rarities"
      options={rarityOptions}
      selectedOptions={cardRarities}
      setSelectedOptionsRemotely={updateRarities}
    />
  </StyledRaritySelector>
);

const StyledRaritySelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default RaritySelector;
