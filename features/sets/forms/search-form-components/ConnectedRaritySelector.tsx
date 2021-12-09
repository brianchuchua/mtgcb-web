import { useDispatch, useSelector } from 'react-redux';
import RaritySelector from '../../../../components/search-form-components/RaritySelector';
import { RootState } from '../../../../redux/rootReducer';
import { CardRarity, rarityOptions, setCardRarities } from '../../setSlice';

const ConnectedRaritySelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateRarities = (selectedRarities: CardRarity[]) => {
    dispatch(setCardRarities({ cardRarities: selectedRarities }));
  };

  const { cardRarities } = useSelector((state: RootState) => state.set);

  return <RaritySelector rarityOptions={rarityOptions} cardRarities={cardRarities} updateRarities={updateRarities} />;
};

export default ConnectedRaritySelector;
