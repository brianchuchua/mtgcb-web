import isMobile from 'is-mobile';
import styled from 'styled-components';
import { CardRarity } from '../../features/browse/browseSlice';
import AutocompleteWithNegation from '../AutocompleteWithNegation';
import NativeMultiselect from '../NativeMultiselect';

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

const RaritySelector: React.FC<RaritySelectorProps> = ({ rarityOptions, cardRarities, updateRarities }) => {
  const isMobileBrowser = isMobile();

  return (
    <StyledRaritySelector>
      {!isMobileBrowser && (
        <AutocompleteWithNegation
          label="Rarities"
          options={rarityOptions}
          selectedOptions={cardRarities}
          setSelectedOptionsRemotely={updateRarities}
        />
      )}
      {isMobileBrowser && (
        <NativeMultiselect
          label="Rarities"
          multiselectOptions={rarityOptions}
          selectedOptions={cardRarities}
          updateSelectedOptions={updateRarities}
        />
      )}
    </StyledRaritySelector>
  );
};

const StyledRaritySelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default RaritySelector;
