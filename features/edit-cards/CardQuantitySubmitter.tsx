import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import { useSnackbar } from 'notistack';
import { useEffect, useState } from 'react';
import Button from '../../components/Button';
import { CardEditingModes } from './editCardsSlice';

export const CardQuantitySubmitter: React.FC<CardQuantitySubmitterProps> = ({
  mode = 'increment',
  cardName,
  quantityReg,
  quantityFoil,
  handleUpdateCollection,
  handleUpdateSuccess,
  isLoading = false,
  isSuccess = false,
  isError = false,
}) => {
  const [internalQuantityReg, setInternalQuantityReg] = useState<number | ''>(mode === 'increment' ? 1 : 0);
  const [internalQuantityFoil, setInternalQuantityFoil] = useState<number | ''>(0);
  const originalQuantityReg = quantityReg;
  const originalQuantityFoil = quantityFoil;

  const { enqueueSnackbar } = useSnackbar();
  useEffect(() => {
    if (isSuccess) {
      const newQuantityReg = mode === 'increment' ? Number(originalQuantityReg) + Number(internalQuantityReg) : internalQuantityReg;
      const newQuantityFoil = mode === 'increment' ? Number(originalQuantityFoil) + Number(internalQuantityFoil) : internalQuantityFoil;
      enqueueSnackbar(
        `${cardName} has been ${mode === 'increment' ? 'incremented' : 'set'} to ${newQuantityReg} (+${newQuantityFoil} foils).`,
        { variant: 'success', anchorOrigin: { horizontal: 'right', vertical: 'bottom' } }
      );
      handleUpdateSuccess();
    }
  }, [isSuccess]);

  const handleQuantityRegChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    if (event.target.value === '') {
      setInternalQuantityReg('');
      return;
    }

    let newQuantityReg = parseInt(event.target.value, 10);

    if (newQuantityReg < 0) {
      newQuantityReg = 0;
    }
    setInternalQuantityReg(newQuantityReg);
  };

  const handleQuantityFoilChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    if (event.target.value === '') {
      setInternalQuantityFoil('');
      return;
    }

    let newQuantityFoil = parseInt(event.target.value, 10);

    if (newQuantityFoil < 0) {
      newQuantityFoil = 0;
    }
    setInternalQuantityFoil(newQuantityFoil);
  };

  const determineLabel = (rootLabel = 'Regular', originalQuantity: number, newQuantity: number | '') => {
    if (mode === 'set' && originalQuantity === newQuantity) {
      return rootLabel;
    }
    if ((mode === 'increment' && newQuantity === 0) || newQuantity === '') {
      return `${rootLabel} (${originalQuantity})`;
    }

    if (mode === 'increment') {
      return `${rootLabel} (${originalQuantity} -> ${originalQuantity + newQuantity})`;
    }
    return `${rootLabel} (${originalQuantity} -> ${newQuantity})`;
  };

  const handleSubmit = () => {
    handleUpdateCollection(internalQuantityReg, internalQuantityFoil, mode);
  };

  return (
    <Grid container spacing={1}>
      <Grid item xs={6}>
        <TextField
          type="number"
          label={determineLabel('Regular', originalQuantityReg, internalQuantityReg)}
          value={internalQuantityReg}
          onChange={handleQuantityRegChange}
          onBlur={() => {
            if (internalQuantityReg === '') {
              setInternalQuantityReg(0);
            }
          }}
          InputLabelProps={{
            shrink: true,
          }}
          margin="dense"
          variant="outlined"
          fullWidth
          error={isError}
          helperText={isError && 'Error -- please try again'}
          autoFocus
          onKeyPress={(e) => {
            if (e.key === 'Enter') {
              handleSubmit();
            }
          }}
        />
      </Grid>
      <Grid item xs={6}>
        <TextField
          type="number"
          label={determineLabel('Foils', originalQuantityFoil, internalQuantityFoil)}
          value={internalQuantityFoil}
          onChange={handleQuantityFoilChange}
          onBlur={() => {
            if (internalQuantityFoil === '') {
              setInternalQuantityFoil(0);
            }
          }}
          InputLabelProps={{
            shrink: true,
          }}
          margin="dense"
          variant="outlined"
          fullWidth
          error={isError}
          helperText={isError && 'Error -- please try again'}
          onKeyPress={(e) => {
            if (e.key === 'Enter') {
              handleSubmit();
            }
          }}
        />
      </Grid>
      <Grid item xs={12} style={{ textAlign: 'center' }}>
        <Button fullWidth variant="contained" color="secondary" isSubmitting={isLoading} onClick={handleSubmit}>
          Save Changes
        </Button>
      </Grid>
    </Grid>
  );
};

interface CardQuantitySubmitterProps {
  mode?: CardEditingModes;
  cardName: string;
  quantityReg: number;
  quantityFoil: number;
  handleUpdateCollection: (quantityReg: number | '', quantityFoil: number | '', mode?: CardEditingModes) => void;
  handleUpdateSuccess: () => void;
  isLoading: boolean;
  isSuccess: boolean;
  isError: boolean;
}
