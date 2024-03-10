import { useDispatch, useSelector } from 'react-redux';
import RaritySelector from '../../../../components/search-form-components/RaritySelector';
import { RootState } from '../../../../redux/rootReducer';
import { convertRaritiesToString } from '../../../../util/queryStringMappers';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { CardRarity, rarityOptions } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedRaritySelectorProps extends ConnectedSearchFormComponentProps {
  setCardRarities: any;
}

const ConnectedRaritySelector: React.FC<ConnectedRaritySelectorProps> = ({ reduxSlice, setCardRarities }) => {
  const dispatch = useDispatch();
  const setQueryParameter = useQueryParameter();

  const updateRarities = (selectedRarities: CardRarity[]) => {
    dispatch(setCardRarities({ cardRarities: selectedRarities }));
    setQueryParameter('rarities', convertRaritiesToString(selectedRarities));
  };

  const { cardRarities } = useSelector((state: RootState) => state[reduxSlice]);

  return <RaritySelector rarityOptions={rarityOptions} cardRarities={cardRarities} updateRarities={updateRarities} />;
};

export default ConnectedRaritySelector;
